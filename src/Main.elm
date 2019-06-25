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
    }


type alias Input =
    { left : Bool
    , right : Bool
    , up : Bool
    }


type GameStatus
    = LoadingAssets
    | GameStarted GameState
    | LoadingFailed


type alias GameState =
    { charSpriteSheet : Texture
    , sprites : { char : Sprites.Char }
    , player : Player
    }


type alias Player =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , ax : Float
    , ay : Float
    , dir : Dir
    , status : PlayerStatus
    }


type PlayerStatus
    = Idle
    | Walking
    | Jumping


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
    | CharacterSpriteLoaded (Maybe Texture)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ onAnimationFrameDelta Frame
                    , onResize Resized
                    , onKeyDown (D.map KeyDown keyDecoder)
                    , onKeyUp (D.map KeyUp keyDecoder)
                    ]
        }


init : Flags -> ( Model, Cmd Msg )
init random =
    ( { count = 0
      , width = 400
      , height = 400
      , game = LoadingAssets
      , input = { left = False, right = False, up = False }
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
                newGame =
                    state |> tick model.width model.height delta model.input
            in
            ( { model
                | count = model.count + delta
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

        ( KeyDown key, _ ) ->
            ( updateKeys key True model, Cmd.none )

        ( KeyUp key, _ ) ->
            ( updateKeys key False model, Cmd.none )

        ( CharacterSpriteLoaded (Just charSpriteSheet), LoadingAssets ) ->
            ( { model
                | game =
                    GameStarted
                        { charSpriteSheet = charSpriteSheet
                        , sprites = { char = Sprites.char charSpriteSheet }
                        , player =
                            { x = model.width / 3
                            , y = 20
                            , vx = 0
                            , vy = 0
                            , ax = 0
                            , ay = 0
                            , dir = R
                            , status = Idle
                            }
                        }
              }
            , Cmd.none
            )

        ( CharacterSpriteLoaded Nothing, LoadingAssets ) ->
            ( { model | game = LoadingFailed }, Cmd.none )

        _ ->
            ( model, Cmd.none )


tick : Float -> Float -> Float -> Input -> GameState -> GameState
tick width height delta input state =
    let
        player =
            state.player

        floorY =
            height - toFloat (Texture.dimensions state.sprites.char.idle).height

        isGrounded =
            state.player.y >= floorY

        friction =
            0.95
                * (if isGrounded then
                    0.95

                   else
                    1
                  )

        gravity =
            50 {- px per second -} * delta / 1000

        xAcc =
            50 {- px per second -} * delta / 1000

        jumpAcc =
            if isGrounded && player.vy >= 0 then
                850 {- px per second -} * delta / 1000

            else if player.vy > 0 then
                gravity / 2

            else
                gravity * (7 / 9)
    in
    { state
        | player =
            { x = player.x + player.vx
            , y =
                (player.y + player.vy)
                    |> min floorY
            , vx = player.vx * friction + player.ax
            , vy = player.vy * friction + player.ay
            , ax =
                if input.left then
                    -xAcc

                else if input.right then
                    xAcc

                else
                    0
            , ay =
                gravity
                    + (if input.up then
                        -jumpAcc

                       else
                        0
                      )
            , dir =
                if input.left then
                    L

                else if input.right then
                    R

                else
                    player.dir
            , status =
                if isGrounded then
                    if abs player.vx > 0.5 || input.left || input.right then
                        Walking

                    else
                        Idle

                else
                    Jumping
            }
    }


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
            , textures =
                [ Texture.loadFromImageUrl
                    "assets/kenney_simplifiedplatformer/Vector/platformPack_character_vector.svg"
                    CharacterSpriteLoaded

                -- , Texture.loadFromImageUrl
                --     "assets/kenney_simplifiedplatformer/Vector/platformPack_tile_vector.svg"
                --     TileSpriteLoad
                ]
            }
            []
            (clearScreen width height :: render count width height game)
        ]


clearScreen width height =
    shapes [ fill Color.lightGray ] [ rect ( 0, 0 ) width height ]


render : Float -> Float -> Float -> GameStatus -> List Renderable
render count width height game =
    case game of
        LoadingAssets ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( width / 2, height / 2 ) "Loading..." ]

        GameStarted { charSpriteSheet, sprites, player } ->
            [ renderPlayer count sprites.char player ]

        LoadingFailed ->
            [ text [ font { family = "sans-serif", size = 48 } ] ( width / 2, height / 2 ) "Loading failed.\nPlease reload." ]


renderPlayer : Float -> Sprites.Char -> Player -> Renderable
renderPlayer count sprites player =
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

        dimensions =
            Texture.dimensions sprite

        centerOffsetX =
            toFloat dimensions.width / 2

        centerOffsetY =
            toFloat dimensions.height / 2
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
