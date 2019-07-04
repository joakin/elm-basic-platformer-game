module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (..)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as D
import Sprites
import Task


type alias Model =
    { count : Float
    , width : Float
    , height : Float
    , game : GameStatus
    , input : Input
    , pageVisibility : Visibility
    }


type alias Input =
    { left : Bool
    , right : Bool
    , up : Bool
    }


type GameStatus
    = LoadingAssets Int { char : Maybe Texture, tile : Maybe Texture }
    | GameStarted GameState
    | LoadingFailed


type alias GameState =
    { assets :
        { charSpriteSheet : Texture
        , tileSpriteSheet : Texture
        , char : Sprites.Char
        , tiles : Sprites.Tile
        }
    , player : Player
    , map : Map
    }


type alias Map =
    List MapTile


type alias MapTile =
    { x : Float
    , y : Float
    , kind : MapTileKind
    , sprite : Texture
    }


type MapTileKind
    = Platform


type alias Player =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , vx : Float
    , vy : Float
    , ax : Float
    , ay : Float
    , dir : Dir
    , grounded : Bool
    , status : PlayerStatus
    , sprites : Sprites.Char
    }


type PlayerStatus
    = Idle
    | Walking
    | Jumping
    | Dead Float


type Dir
    = L
    | R


type alias Flags =
    Float


type Msg
    = Frame Float
    | GetViewport Viewport
    | Resized Int Int
    | KeyDown String
    | KeyUp String
    | AssetLoaded AssetKind (Maybe Texture)
    | PageVisibilityChanged Visibility


type AssetKind
    = Char
    | Tile


textures =
    [ Texture.loadFromImageUrl
        "assets/kenney_simplifiedplatformer/Vector/platformPack_character_vector.svg"
        (AssetLoaded Char)
    , Texture.loadFromImageUrl
        "assets/kenney_simplifiedplatformer/Vector/platformPack_tile_vector.svg"
        (AssetLoaded Tile)
    ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize Resized
        , onVisibilityChange PageVisibilityChanged
        , onAnimationFrameDelta Frame
        , onKeyDown (D.map KeyDown keyDecoder)
        , onKeyUp (D.map KeyUp keyDecoder)
        ]


init : Flags -> ( Model, Cmd Msg )
init random =
    ( { count = 0
      , width = 400
      , height = 400
      , game = LoadingAssets (List.length textures) { char = Nothing, tile = Nothing }
      , input = { left = False, right = False, up = False }
      , pageVisibility = Visible
      }
    , Task.perform GetViewport getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ =
    --         Debug.log "msg, model" ( msg, model )
    -- in
    -- Debug.log "ret" <|
    case ( msg, model.game ) of
        ( Frame delta, GameStarted state ) ->
            let
                count =
                    model.count + delta

                newGame =
                    state |> tick model.width model.height count delta model.input

                -- _ =
                --     Debug.log "player" state.player
                -- _ =
                --     Debug.log "<- player" newGame.player
            in
            ( { model
                | count = count
                , game = GameStarted newGame
              }
            , Cmd.none
            )

        ( GetViewport data, _ ) ->
            ( { model
                | width = data.viewport.width
                , height = data.viewport.height
              }
            , Cmd.none
            )

        ( Resized width height, _ ) ->
            ( { model | width = toFloat width, height = toFloat height }
            , Cmd.none
            )

        ( PageVisibilityChanged vis, _ ) ->
            ( { model | pageVisibility = vis }, Cmd.none )

        ( KeyDown key, _ ) ->
            ( updateKeys key True model, Cmd.none )

        ( KeyUp key, _ ) ->
            ( updateKeys key False model, Cmd.none )

        ( AssetLoaded assetKind maybeAsset, LoadingAssets remaining_ assets_ ) ->
            let
                assets =
                    case assetKind of
                        Char ->
                            { assets_ | char = maybeAsset }

                        Tile ->
                            { assets_ | tile = maybeAsset }

                remaining =
                    remaining_ - 1
            in
            ( if remaining == 0 then
                Maybe.map2
                    (\charSpriteSheet tileSpriteSheet ->
                        let
                            char =
                                Sprites.char charSpriteSheet

                            tiles =
                                Sprites.tile tileSpriteSheet
                        in
                        { model
                            | game =
                                GameStarted
                                    { assets =
                                        { charSpriteSheet = charSpriteSheet
                                        , tileSpriteSheet = tileSpriteSheet
                                        , char = char
                                        , tiles = tiles
                                        }
                                    , player =
                                        let
                                            { width, height } =
                                                Texture.dimensions char.idle
                                        in
                                        { x = 100
                                        , y = 20
                                        , w = width
                                        , h = height
                                        , vx = 0
                                        , vy = 0
                                        , ax = 0
                                        , ay = 0
                                        , dir = R
                                        , status = Idle
                                        , grounded = False
                                        , sprites = char
                                        }
                                    , map =
                                        let
                                            { width, height } =
                                                Texture.dimensions tiles.soilGrass
                                        in
                                        [ { kind = Platform, x = 50 + 0 * width, y = 450, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 1 * width, y = 450, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 2 * width, y = 480, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 3 * width, y = 550, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 4 * width, y = 650, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 5 * width, y = 600, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 6 * width, y = 550, sprite = tiles.soilGrass }
                                        , { kind = Platform, x = 50 + 7 * width, y = 500, sprite = tiles.soilGrass }
                                        ]
                                    }
                        }
                    )
                    assets.char
                    assets.tile
                    |> Maybe.withDefault { model | game = LoadingFailed }

              else
                { model | game = LoadingAssets remaining assets }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


gravity delta =
    50 {- px per second -} * delta / 1000


tick : Float -> Float -> Float -> Float -> Input -> GameState -> GameState
tick width height count delta input state =
    let
        player =
            state.player

        playerDimensions =
            Texture.dimensions state.assets.char.idle

        floorY =
            height - playerDimensions.height

        outOfScreen =
            player.y >= floorY

        deathAcc =
            55

        ded =
            case player.status of
                Dead f ->
                    True

                _ ->
                    False
    in
    (if ded then
        state

     else if outOfScreen then
        { state | player = { player | status = Dead count, ay = -deathAcc } }

     else
        let
            wallRight =
                height - playerDimensions.height

            xAcc =
                50 {- px per second -} * delta / 1000

            jumpAcc =
                if player.grounded then
                    10

                else if player.vy > 0 then
                    gravity delta / 2

                else
                    gravity delta * (7 / 9)
        in
        { state
            | player =
                { player
                    | ax =
                        if input.left then
                            -xAcc

                        else if input.right then
                            xAcc

                        else
                            0
                    , ay =
                        if input.up then
                            -jumpAcc

                        else
                            0
                    , dir =
                        if input.left then
                            L

                        else if input.right then
                            R

                        else
                            player.dir
                    , status =
                        if player.grounded then
                            if abs player.vx > 0.5 || input.left || input.right then
                                Walking

                            else
                                Idle

                        else
                            Jumping
                }
        }
    )
        |> physics delta


collisions : GameState -> GameState -> GameState
collisions oldState ({ player, map } as state) =
    let
        ( newPlayerY, newMapY ) =
            List.foldr (collisionStep moveY) ( { player | grounded = False }, [] ) map

        ( newPlayer, newMap ) =
            List.foldr (collisionStep moveX) ( newPlayerY, [] ) newMapY
    in
    { state | player = newPlayer, map = newMap }


collisionStep : (Player -> MapTile -> ( Player, MapTile )) -> MapTile -> ( Player, Map ) -> ( Player, Map )
collisionStep resolve tile ( player, map ) =
    case tile.kind of
        Platform ->
            let
                { width, height } =
                    Texture.dimensions tile.sprite

                ( newPlayer, newTile ) =
                    resolve player tile
            in
            ( newPlayer
            , newTile :: map
            )


collides : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
collides x1 y1 w1 h1 x2 y2 w2 h2 =
    if
        (x1 + w1 > x2)
            && (x1 < x2 + w2)
            && (y1 + h1 > y2)
            && (y1 < y2 + h2)
    then
        True

    else
        False


standingOn : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
standingOn x1 y1 w1 h1 x2 y2 w2 h2 =
    if
        (x1 + w1 > x2)
            && (x1 < x2 + w2)
            && (y1 + h1 >= y2)
            && (y1 < y2 + h2)
    then
        True

    else
        False


moveX : Player -> MapTile -> ( Player, MapTile )
moveX pl tile =
    let
        { width, height } =
            Texture.dimensions tile.sprite
    in
    if collides pl.x pl.y pl.w pl.h tile.x tile.y width height then
        if pl.vx < 0 then
            if tile.x + width < pl.x + pl.w then
                ( { pl | vx = 0, x = tile.x + width }, tile )

            else
                ( pl, tile )

        else if tile.x > pl.x then
            ( { pl | vx = 0, x = tile.x - pl.w }, tile )

        else
            ( pl, tile )

    else
        ( pl, tile )


moveY : Player -> MapTile -> ( Player, MapTile )
moveY pl tile =
    let
        { width, height } =
            Texture.dimensions tile.sprite
    in
    if collides pl.x pl.y pl.w pl.h tile.x tile.y width height then
        if pl.vy < 0 && tile.y < pl.y then
            ( { pl | vy = 0, y = tile.y + height }, tile )

        else if tile.y + height > pl.y + pl.h then
            ( { pl | grounded = True, vy = 0, y = tile.y - pl.h }, tile )

        else
            ( pl, tile )

    else if standingOn pl.x pl.y pl.w pl.h tile.x tile.y width height then
        ( { pl | grounded = True }, tile )

    else
        ( pl, tile )


physics : Float -> GameState -> GameState
physics delta ({ player } as state) =
    let
        friction =
            0.95
                * (if player.grounded then
                    0.95

                   else
                    1
                  )

        newState =
            { state
                | player =
                    { player
                        | x = player.x + player.vx
                        , y = player.y + player.vy
                        , vx = player.vx * friction + player.ax
                        , vy = player.vy * friction + player.ay + gravity delta
                        , ax = 0
                        , ay = 0
                    }
            }
    in
    collisions state newState


updateKeys : String -> Bool -> Model -> Model
updateKeys key isDown model =
    let
        input =
            model.input
    in
    case key of
        "ArrowUp" ->
            { model | input = { input | up = isDown } }

        "ArrowLeft" ->
            { model | input = { input | left = isDown } }

        "ArrowRight" ->
            { model | input = { input | right = isDown } }

        _ ->
            model


view : Model -> Html Msg
view { count, width, height, game } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtmlWith
            { width = round width
            , height = round height
            , textures = textures
            }
            []
            (clearScreen width height :: render count width height game)
        ]


clearScreen width height =
    shapes [ fill Color.lightGray ] [ rect ( 0, 0 ) width height ]


render : Float -> Float -> Float -> GameStatus -> List Renderable
render count width height game =
    case game of
        LoadingAssets _ _ ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( width / 2, height / 2 ) "Loading..." ]

        GameStarted { assets, player, map } ->
            renderMap map
                ++ [ renderPlayer count player ]

        LoadingFailed ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( width / 2, height / 2 ) "Loading failed.\nPlease reload." ]


renderMap : Map -> List Renderable
renderMap map =
    List.map
        (\t ->
            texture [] ( t.x, t.y ) t.sprite
        )
        map


renderPlayer : Float -> Player -> Renderable
renderPlayer count ({ sprites } as player) =
    let
        sprite =
            case player.status of
                Jumping ->
                    sprites.jump

                Walking ->
                    if sin (count / 40) < 0 then
                        sprites.walk0

                    else
                        sprites.walk1

                Idle ->
                    sprites.idle

                Dead frame ->
                    sprites.crouch

        rotation =
            case player.status of
                Dead frame ->
                    (count - frame) / 100

                _ ->
                    0

        dimensions =
            Texture.dimensions sprite

        centerOffsetX =
            dimensions.width / 2

        centerOffsetY =
            dimensions.height / 2
    in
    texture
        [ transform
            [ translate (player.x + centerOffsetX) (player.y + centerOffsetY)
            , scale
                (case player.dir of
                    L ->
                        -1

                    R ->
                        1
                )
                1
            , rotate rotation
            ]
        ]
        ( -centerOffsetX, -centerOffsetY )
        sprite


debugBox pos w h =
    shapes [ alpha 0.3, stroke Color.red, fill Color.lightRed ]
        [ rect pos w h ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


roundFloat : Float -> Float
roundFloat f =
    (f * 1000000 |> round |> toFloat) / 1000000
