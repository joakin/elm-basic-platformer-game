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
import Physics
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
        , tiles : Sprites.Tiles
        }
    , player : Player
    , map : Map
    }


type alias Map =
    List MapTile


type alias MapTile =
    { physics : Physics.Object
    , kind : MapTileKind
    , texture : Texture
    }


type MapTileKind
    = Platform


type alias Player =
    { physics : Physics.Object
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
    = Left
    | Right


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
update msg ({ input, count, width, height, game } as model) =
    -- let
    --     _ =
    --         Debug.log "msg, model" ( msg, model )
    -- in
    -- Debug.log "ret" <|
    case ( msg, game ) of
        ( Frame delta, GameStarted state ) ->
            let
                newCount =
                    count + delta

                newGame =
                    state |> tick width height newCount delta input
            in
            ( { model
                | count = newCount
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

        ( Resized newWidth newHeight, _ ) ->
            ( { model | width = toFloat newWidth, height = toFloat newHeight }
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
                                Sprites.tiles tileSpriteSheet
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
                                            cd =
                                                Texture.dimensions char.idle
                                        in
                                        { physics =
                                            { x = 100
                                            , y = 20
                                            , w = cd.width
                                            , h = cd.height
                                            , bx = 15
                                            , by = 15
                                            , bw = cd.width - 15 * 2
                                            , bh = cd.height - 15
                                            , vx = 0
                                            , vy = 0
                                            , ax = 0
                                            , ay = 0
                                            }
                                        , dir = Right
                                        , status = Idle
                                        , grounded = False
                                        , sprites = char
                                        }
                                    , map =
                                        let
                                            tile get mapTileKind x y =
                                                let
                                                    sprite =
                                                        get tiles

                                                    d =
                                                        Texture.dimensions sprite.texture
                                                in
                                                { kind = mapTileKind
                                                , physics =
                                                    { emptyObject
                                                        | x = x
                                                        , y = y
                                                        , w = d.width
                                                        , h = d.height
                                                        , bx = sprite.bx
                                                        , by = sprite.by
                                                        , bw = sprite.bw
                                                        , bh = sprite.bh
                                                    }
                                                , texture = sprite.texture
                                                }

                                            emptyObject =
                                                Physics.emptyObject
                                        in
                                        [ tile .soilGrass Platform (50 + 0 * 64) 450
                                        , tile .soilGrass Platform (50 + 1 * 64) 450
                                        , tile .soilGrass Platform (50 + 2 * 64) 480
                                        , tile .soilGrass Platform (50 + 3 * 64) 550
                                        , tile .soilGrass Platform (50 + 4 * 64) 650
                                        , tile .soilGrass Platform (50 + 5 * 64) 600
                                        , tile .soilGrass Platform (50 + 6 * 64) 550
                                        , tile .soilGrass Platform (50 + 7 * 64) 500
                                        , tile .soilGrassPlatform Platform (50 + 3 * 64) 300
                                        , tile .soilGrassPlatform Platform (50 + 6 * 64) 200
                                        , tile .soilGrassPlatform Platform (50 + 7 * 64) 200
                                        , tile .soilGrassPlatform Platform (50 + 8 * 64) 200
                                        , tile .soilSand Platform (700 + 0 * 64) 450
                                        , tile .soilSand Platform (700 + 1 * 64) 450
                                        , tile .soilSand Platform (700 + 2 * 64) 480
                                        , tile .soilSand Platform (700 + 3 * 64) 550
                                        , tile .soilSand Platform (700 + 4 * 64) 650
                                        , tile .soilSand Platform (700 + 5 * 64) 600
                                        , tile .soilSand Platform (700 + 6 * 64) 550
                                        , tile .soilSand Platform (700 + 7 * 64) 500
                                        , tile .soilSandPlatform Platform (700 + 3 * 64) 300
                                        , tile .soilSandPlatform Platform (700 + 6 * 64) 200
                                        , tile .soilSandPlatform Platform (700 + 7 * 64) 200
                                        , tile .soilSandPlatform Platform (700 + 8 * 64) 200
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
    70 {- px per second -} * delta / 1000


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
            player.physics.y >= floorY

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
        { state | player = { player | status = Dead count, physics = Physics.push 0 -deathAcc player.physics } }

     else
        let
            wallRight =
                height - playerDimensions.height

            xAcc =
                50 {- px per second -} * delta / 1000

            jumpAcc =
                if player.grounded && player.physics.vy >= 0 then
                    20

                else if player.physics.vy > 0 then
                    gravity delta / 3

                else
                    gravity delta * (7 / 9)

            xForce =
                if input.left then
                    -xAcc

                else if input.right then
                    xAcc

                else
                    0

            yForce =
                if input.up then
                    -jumpAcc

                else
                    0
        in
        { state
            | player =
                { player
                    | physics =
                        Physics.push xForce yForce player.physics
                    , dir =
                        if input.left then
                            Left

                        else if input.right then
                            Right

                        else
                            player.dir
                    , status =
                        if player.grounded then
                            if abs player.physics.vx > 0.5 || input.left || input.right then
                                Walking

                            else
                                Idle

                        else
                            Jumping
                }
        }
    )
        |> physics delta


collisions : GameState -> GameState
collisions ({ player, map } as state) =
    let
        ( newPlayer, newMap ) =
            List.foldr collisionStep ( { player | grounded = False }, [] ) map
    in
    { state | player = newPlayer, map = newMap }


collisionStep : MapTile -> ( Player, Map ) -> ( Player, Map )
collisionStep tile ( player, map ) =
    case tile.kind of
        Platform ->
            let
                ( maybeCollisionDirection, movedPlayerObj ) =
                    Physics.collisionCheck player.physics tile.physics

                newPlayer =
                    case maybeCollisionDirection of
                        Just Physics.B ->
                            { player | grounded = True, physics = { movedPlayerObj | vy = 0 } }

                        Just Physics.T ->
                            { player | physics = { movedPlayerObj | vy = 0 } }

                        Just Physics.R ->
                            { player | physics = { movedPlayerObj | vx = 0 } }

                        Just Physics.L ->
                            { player | physics = { movedPlayerObj | vx = 0 } }

                        Nothing ->
                            { player | physics = movedPlayerObj }
            in
            ( if newPlayer.physics |> Physics.standingOn tile.physics then
                { newPlayer | grounded = True }

              else
                newPlayer
            , tile :: map
            )


physics : Float -> GameState -> GameState
physics delta ({ player } as state) =
    let
        frictionX =
            if player.grounded then
                0.8835

            else
                0.9

        frictionY =
            0.95

        newState =
            { state
                | player =
                    { player
                        | physics = Physics.integrate frictionX frictionY (gravity delta) player.physics
                    }
            }
    in
    collisions newState


updateKeys : String -> Bool -> Model -> Model
updateKeys key isDown ({ input } as model) =
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
            -- Sprites.tileSpriteSheetDebugRenderable assets.tileSpriteSheet ++
            renderMap map
                ++ [ renderPlayer count player

                   -- , debugBox ( player.physics.x + player.physics.bx, player.physics.y + player.physics.by ) player.physics.bw player.physics.bh
                   ]

        LoadingFailed ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( width / 2, height / 2 ) "Loading failed.\nPlease reload." ]


renderMap : Map -> List Renderable
renderMap map =
    List.map
        (\t ->
            texture [] ( t.physics.x, t.physics.y ) t.texture
        )
        -- map
        -- ++ List.map
        --     (\t ->
        --         debugBox ( t.physics.x + t.physics.bx, t.physics.y + t.physics.by ) t.physics.bw t.physics.bh
        --     )
        map


renderPlayer : Float -> Player -> Renderable
renderPlayer count ({ sprites } as player) =
    let
        playerTexture =
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
            Texture.dimensions playerTexture

        centerOffsetX =
            dimensions.width / 2

        centerOffsetY =
            dimensions.height / 2
    in
    texture
        [ transform
            [ translate (player.physics.x + centerOffsetX) (player.physics.y + centerOffsetY)
            , scale
                (case player.dir of
                    Left ->
                        -1

                    Right ->
                        1
                )
                1
            , rotate rotation
            ]
        ]
        ( -centerOffsetX, -centerOffsetY )
        playerTexture


debugBox pos w h =
    shapes [ alpha 0.3, stroke Color.red, fill Color.lightRed ]
        [ rect pos w h ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


roundFloat : Float -> Float
roundFloat f =
    (f * 1000000 |> round |> toFloat) / 1000000
