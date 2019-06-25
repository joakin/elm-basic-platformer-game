module Sprites exposing (Char, char)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color


type alias Char =
    { idle : Texture
    , jump : Texture
    , walk0 : Texture
    , walk1 : Texture
    , climb0 : Texture
    , climb1 : Texture
    , crouch : Texture
    , win : Texture
    }


charSprite : Int -> Int -> Texture -> Texture
charSprite x y spriteSheet =
    let
        paddingLeft =
            28

        paddingTop =
            4

        spriteSize =
            82

        gap =
            14

        vgap =
            14
    in
    Texture.sprite
        { x = x * (spriteSize + gap) + paddingLeft
        , y = y * (spriteSize + vgap) + paddingTop
        , width = spriteSize
        , height = spriteSize
        }
        spriteSheet


charSpriteSheetDebugRenderable : Texture -> List Renderable
charSpriteSheetDebugRenderable charSpriteSheet =
    [ texture [] ( 0, 0 ) charSpriteSheet ]
        ++ (List.range 0 7
                |> List.map
                    (\i ->
                        let
                            paddingLeft =
                                28

                            paddingTop =
                                4

                            spriteSize =
                                82

                            gap =
                                14

                            vgap =
                                14
                        in
                        shapes [ alpha 0.3, stroke Color.red, fill Color.lightRed ]
                            [ rect
                                ( (i |> modBy 4) * (spriteSize + gap) + paddingLeft |> toFloat
                                , i // 4 * (spriteSize + vgap) + paddingTop |> toFloat
                                )
                                spriteSize
                                spriteSize
                            ]
                    )
           )


char : Texture -> Char
char spriteSheet =
    { idle = charSprite 0 0 spriteSheet
    , jump = charSprite 1 0 spriteSheet
    , walk0 = charSprite 2 0 spriteSheet
    , walk1 = charSprite 3 0 spriteSheet
    , climb0 = charSprite 0 1 spriteSheet
    , climb1 = charSprite 1 1 spriteSheet
    , crouch = charSprite 2 1 spriteSheet
    , win = charSprite 3 1 spriteSheet
    }
