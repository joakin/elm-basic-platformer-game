module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (..)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Json.Decode as D
import List.Extra as List
import Physics
import Sprites
import Task


type alias Model =
    { count : Float
    , screen : Screen
    , game : GameStatus
    , input : Input
    , pageVisibility : Visibility
    }


type alias Screen =
    { width : Float
    , height : Float
    }


type alias Input =
    { left : Bool
    , right : Bool
    , up : Bool
    , touchLeft : Maybe { started : Float, origin : { x : Float, y : Float }, current : { x : Float, y : Float } }
    , touchRight : Maybe { started : Float }
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
    , camera : Camera
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
    | Deadly


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
    | TouchStart Touch.Event
    | TouchMove Touch.Event
    | TouchEnd Touch.Event
    | TouchCancel Touch.Event
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


type alias Camera =
    { x : Float, y : Float }


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
      , screen = { width = 400, height = 400 }
      , game = LoadingAssets (List.length textures) { char = Nothing, tile = Nothing }
      , input = { left = False, right = False, up = False, touchLeft = Nothing, touchRight = Nothing }
      , pageVisibility = Visible
      }
    , Task.perform GetViewport getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ input, count, screen, game } as model) =
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
                    state |> tick screen newCount delta input
            in
            ( { model
                | count = newCount
                , game = GameStarted newGame
              }
            , Cmd.none
            )

        ( GetViewport data, _ ) ->
            ( { model
                | screen =
                    { width = data.viewport.width
                    , height = data.viewport.height
                    }
              }
            , Cmd.none
            )

        ( Resized newWidth newHeight, _ ) ->
            ( { model
                | screen =
                    { width = toFloat newWidth
                    , height = toFloat newHeight
                    }
              }
            , Cmd.none
            )

        ( PageVisibilityChanged vis, _ ) ->
            ( { model | pageVisibility = vis }, Cmd.none )

        ( KeyDown key, _ ) ->
            ( updateKeys key True model, Cmd.none )

        ( KeyUp key, _ ) ->
            ( updateKeys key False model, Cmd.none )

        ( TouchStart event, _ ) ->
            ( updateTouches event model, Cmd.none )

        ( TouchMove event, _ ) ->
            ( updateTouches event model, Cmd.none )

        ( TouchEnd event, _ ) ->
            ( updateTouches event model, Cmd.none )

        ( TouchCancel event, _ ) ->
            ( updateTouches event model, Cmd.none )

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
                                    ({ assets =
                                        { charSpriteSheet = charSpriteSheet
                                        , tileSpriteSheet = tileSpriteSheet
                                        , char = char
                                        , tiles = tiles
                                        }
                                     , camera =
                                        { x = 0
                                        , y = 0
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
                                        (List.range -50 50
                                            |> List.concatMap
                                                (\i ->
                                                    [ tile .lavaSurface Deadly (toFloat i * 64) 750
                                                    , tile .lava Deadly (toFloat i * 64) (750 + 64)
                                                    , tile .lava Deadly (toFloat i * 64) (750 + 64 * 2)
                                                    ]
                                                )
                                        )
                                            ++ [ tile .soilGrass Platform (50 + 0 * 64) 450
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
                                               , tile .soil Platform (700 + 1 * 64) 450
                                               , tile .soil Platform (700 + 1 * 64) (450 - 1 * 64)
                                               , tile .soil Platform (700 + 1 * 64) (450 - 2 * 64)
                                               , tile .soil Platform (700 + 1 * 64) (450 - 3 * 64)
                                               , tile .soil Platform (700 + 1 * 64) (450 - 4 * 64)
                                               , tile .soilSand Platform (700 + 1 * 64) (450 - 5 * 64)
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
                                        |> syncCamera screen
                                    )
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


deathAcc =
    30


tick : Screen -> Float -> Float -> Input -> GameState -> GameState
tick screen count delta input state =
    let
        player =
            state.player

        ded =
            case player.status of
                Dead f ->
                    True

                _ ->
                    False
    in
    (if ded then
        state

     else
        let
            xAcc =
                if player.grounded then
                    35 {- px per second -} * delta / 1000

                else
                    15 {- px per second -} * delta / 1000

            jumpAcc =
                if player.grounded && player.physics.vy >= 0 then
                    8

                else if player.physics.vy > 0 then
                    gravity delta / 2.5

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
        |> physics count delta
        |> syncCamera screen


syncCamera : Screen -> GameState -> GameState
syncCamera screen ({ player, camera } as state) =
    { state
        | camera =
            { x = player.physics.x + player.physics.w / 2 - screen.width / 2
            , y = player.physics.y + player.physics.h / 2 - screen.height / 2
            }
    }


collisions : Float -> GameState -> GameState
collisions count ({ player, map } as state) =
    let
        ( newPlayer, newMap ) =
            List.foldr (collisionStep count) ( { player | grounded = False }, [] ) map
    in
    { state | player = newPlayer, map = newMap }


collisionStep : Float -> MapTile -> ( Player, Map ) -> ( Player, Map )
collisionStep count tile ( player, map ) =
    case player.status of
        Dead _ ->
            case tile.kind of
                Deadly ->
                    let
                        ( maybeCollisionDirection, movedPlayerPhysics ) =
                            Physics.collisionCheck player.physics tile.physics

                        newPlayer =
                            case maybeCollisionDirection of
                                Just _ ->
                                    { player | status = Dead count, physics = movedPlayerPhysics }

                                Nothing ->
                                    player
                    in
                    ( newPlayer
                    , tile :: map
                    )

                _ ->
                    ( player, tile :: map )

        _ ->
            case tile.kind of
                Platform ->
                    let
                        ( maybeCollisionDirection, movedPlayerPhysics ) =
                            Physics.collisionCheck player.physics tile.physics

                        newPlayer =
                            case maybeCollisionDirection of
                                Just Physics.B ->
                                    { player | grounded = True, physics = { movedPlayerPhysics | vy = 0 } }

                                Just Physics.T ->
                                    { player | physics = { movedPlayerPhysics | vy = 0 } }

                                Just Physics.R ->
                                    { player | physics = { movedPlayerPhysics | vx = 0 } }

                                Just Physics.L ->
                                    { player | physics = { movedPlayerPhysics | vx = 0 } }

                                Nothing ->
                                    player
                    in
                    ( if newPlayer.physics |> Physics.standingOn tile.physics then
                        { newPlayer | grounded = True }

                      else
                        newPlayer
                    , tile :: map
                    )

                Deadly ->
                    let
                        ( maybeCollisionDirection, movedPlayerPhysics ) =
                            Physics.collisionCheck player.physics tile.physics

                        newPlayer =
                            case maybeCollisionDirection of
                                Just _ ->
                                    { player | status = Dead count, physics = Physics.push 0 -deathAcc movedPlayerPhysics }

                                Nothing ->
                                    player
                    in
                    ( newPlayer
                    , tile :: map
                    )


physics : Float -> Float -> GameState -> GameState
physics count remainingDelta ({ player } as state) =
    let
        maxDelta =
            6

        delta =
            min remainingDelta maxDelta

        newRemainingDelta =
            max 0 (remainingDelta - maxDelta)

        frictionX =
            if player.grounded then
                0.92

            else
                0.99

        frictionY =
            0.985

        maxVX =
            300 {- px / second -} * 16 / 1000

        -- 50 * 16 / 1000 = 0.8
        newState =
            { state
                | player =
                    { player
                        | physics = Physics.integrate frictionX frictionY maxVX (gravity delta) player.physics
                    }
            }
                |> collisions count
    in
    if newRemainingDelta > 0 then
        physics count newRemainingDelta newState

    else
        newState


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


updateTouches : Touch.Event -> Model -> Model
updateTouches event ({ screen, input, count } as model) =
    let
        findTouchInCoords x y w h =
            List.find
                (\t ->
                    let
                        ( tx, ty ) =
                            t.clientPos
                    in
                    tx > x && tx < x + w && ty > y && ty < y + h
                )
                event.touches

        newInput =
            { input
                | touchLeft =
                    findTouchInCoords 0 0 (screen.width * 2 / 3) screen.height
                        |> Maybe.map
                            (\t ->
                                case input.touchLeft of
                                    Nothing ->
                                        { started = count
                                        , origin =
                                            { x = t.clientPos |> Tuple.first
                                            , y = t.clientPos |> Tuple.second
                                            }
                                        , current =
                                            { x = t.clientPos |> Tuple.first
                                            , y = t.clientPos |> Tuple.second
                                            }
                                        }

                                    Just tl ->
                                        { tl
                                            | current =
                                                { x = t.clientPos |> Tuple.first
                                                , y = t.clientPos |> Tuple.second
                                                }
                                        }
                            )
                , touchRight =
                    findTouchInCoords (screen.width * 2 / 3) 0 (screen.width / 3) screen.height
                        |> Maybe.andThen
                            (\_ ->
                                case input.touchRight of
                                    Nothing ->
                                        Just { started = count }

                                    _ ->
                                        input.touchRight
                            )
            }

        newInputWithTouchRight =
            { newInput
                | up =
                    case newInput.touchRight of
                        Just right ->
                            True

                        Nothing ->
                            False
            }
    in
    { model
        | input =
            case newInputWithTouchRight.touchLeft of
                Just { started, origin, current } ->
                    if abs (current.x - origin.x) > touchMoveThreshold then
                        if current.x > origin.x then
                            { newInputWithTouchRight | left = False, right = True }

                        else
                            { newInputWithTouchRight | left = True, right = False }

                    else
                        { newInputWithTouchRight | left = False, right = False }

                Nothing ->
                    { newInputWithTouchRight | left = False, right = False }
    }


touchMoveThreshold =
    30


view : Model -> Html Msg
view { count, screen, game } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtmlWith
            { width = round screen.width
            , height = round screen.height
            , textures = textures
            }
            [ Touch.onStart TouchStart
            , Touch.onMove TouchMove
            , Touch.onEnd TouchEnd
            , Touch.onCancel TouchCancel
            ]
            (clearScreen screen.width screen.height :: render count screen game)
        ]


clearScreen width height =
    shapes [ fill Color.lightGray ] [ rect ( 0, 0 ) width height ]


render : Float -> Screen -> GameStatus -> List Renderable
render count screen game =
    case game of
        LoadingAssets _ _ ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( screen.width / 2, screen.height / 2 ) "Loading..." ]

        GameStarted { assets, camera, player, map } ->
            -- Sprites.tileSpriteSheetDebugRenderable assets.tileSpriteSheet ++
            renderMap screen camera map
                ++ [ renderPlayer screen count camera player

                   -- , debugBox ( player.physics.x + player.physics.bx, player.physics.y + player.physics.by ) player.physics.bw player.physics.bh
                   ]

        LoadingFailed ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( screen.width / 2, screen.height / 2 ) "Loading failed.\nPlease reload." ]


renderMap : Screen -> Camera -> Map -> List Renderable
renderMap screen camera map =
    map
        |> List.filter (\t -> visible screen camera t.physics)
        |> List.map (\t -> texture [] (screenCoords screen camera t.physics) t.texture)



-- ++ List.map
--     (\t ->
--         debugBox ( t.physics.x + t.physics.bx, t.physics.y + t.physics.by ) t.physics.bw t.physics.bh
--     )
--     map


renderPlayer : Screen -> Float -> Camera -> Player -> Renderable
renderPlayer screen count camera ({ sprites } as player) =
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

        ( x, y ) =
            screenCoords screen camera player.physics

        centerOffsetX =
            dimensions.width / 2

        centerOffsetY =
            dimensions.height / 2
    in
    texture
        [ transform
            [ translate (x + centerOffsetX) (y + centerOffsetY)
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


visible : Screen -> Camera -> { a | x : Float, y : Float, w : Float, h : Float } -> Bool
visible screen camera obj =
    (obj.x + obj.w > camera.x)
        && (obj.x < camera.x + screen.width)
        && (obj.y + obj.h > camera.y)
        && (obj.y < camera.y + screen.height)


screenCoords : Screen -> Camera -> { a | x : Float, y : Float } -> ( Float, Float )
screenCoords screen camera obj =
    ( obj.x - camera.x, obj.y - camera.y )


debugBox pos w h =
    shapes [ alpha 0.3, stroke Color.red, fill Color.lightRed ]
        [ rect pos w h ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


roundFloat : Float -> Float
roundFloat f =
    (f * 1000000 |> round |> toFloat) / 1000000
