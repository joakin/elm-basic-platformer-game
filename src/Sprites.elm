module Sprites exposing
    ( Char
    , Tile
    , allTiles
    , char
    , charSpriteSheetDebugRenderable
    , tile
    , tileSpriteSheetDebugRenderable
    )

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


charSprite : Float -> Float -> Texture -> Texture
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


type alias Tile =
    { allowedBlockPlaceholder : Texture
    , blockedBlockPlaceholder : Texture
    , blueBlock : Texture
    , blueBlockPlaceholder : Texture
    , blueDiamond : Texture
    , blueDoor : Texture
    , blueDoorTop : Texture
    , blueGem : Texture
    , blueKey : Texture
    , bluePortal : Texture
    , blueSwitch : Texture
    , blueSwitchPressed : Texture
    , concrete : Texture
    , concreteBlock : Texture
    , concreteBricksBlock : Texture
    , concreteGrass : Texture
    , concreteGrassPlatform : Texture
    , concreteIce : Texture
    , concreteIcePlatform : Texture
    , concreteSand : Texture
    , concreteSandPlaform : Texture
    , diamondPlaceholder : Texture
    , emptyBlockPlaceholder : Texture
    , emptyHeart : Texture
    , gemPlaceholder : Texture
    , goldBlock : Texture
    , goldBlockGemHole : Texture
    , grass : Texture
    , greenBlock : Texture
    , greenBlockPlaceholder : Texture
    , greenDiamond : Texture
    , greenDoor : Texture
    , greenDoorTop : Texture
    , greenGem : Texture
    , greenKey : Texture
    , greenPortal : Texture
    , greenSwitch : Texture
    , greenSwitchPressed : Texture
    , halfHeart : Texture
    , heart : Texture
    , keyPlaceholder : Texture
    , ladder : Texture
    , ladderTop : Texture
    , lava : Texture
    , lavaSurface : Texture
    , openDoor : Texture
    , openDoorTop : Texture
    , redBlock : Texture
    , redBlockPlaceholder : Texture
    , redDiamond : Texture
    , redDoor : Texture
    , redDoorTop : Texture
    , redGem : Texture
    , redKey : Texture
    , redPortal : Texture
    , redSwitch : Texture
    , redSwitchPressed : Texture
    , sawBlade : Texture
    , soil : Texture
    , soilGrass : Texture
    , soilGrassPlatform : Texture
    , soilIce : Texture
    , soilIcePlatform : Texture
    , soilSand : Texture
    , soilSandPlatform : Texture
    , spikes : Texture
    , water : Texture
    , waterSurface : Texture
    , woodBlock : Texture
    , woodBricksBlock : Texture
    , woodCrate : Texture
    , woodFootbridge : Texture
    , woodFrame : Texture
    , yellowBlock : Texture
    , yellowBlockPlaceholder : Texture
    , yellowDiamond : Texture
    , yellowDoor : Texture
    , yellowDoorTop : Texture
    , yellowGem : Texture
    , yellowKey : Texture
    , yellowPortal : Texture
    , yellowSwitch : Texture
    , yellowSwitchPressed : Texture
    }


tileSpriteSheetDebugRenderable : Texture -> List Renderable
tileSpriteSheetDebugRenderable tileSpriteSheet =
    texture [] ( 0, 0 ) tileSpriteSheet
        :: (List.range 0 (16 * 6 + 1)
                |> List.map
                    (\i ->
                        let
                            paddingLeft =
                                40

                            paddingTop =
                                40

                            spriteSize =
                                64

                            gap =
                                32

                            vgap =
                                32
                        in
                        shapes [ alpha 0.3, stroke Color.red, fill Color.lightRed ]
                            [ rect
                                ( (i |> modBy 14) * (spriteSize + gap) + paddingLeft |> toFloat
                                , i // 14 * (spriteSize + vgap) + paddingTop |> toFloat
                                )
                                spriteSize
                                spriteSize
                            ]
                    )
           )


tileSprite : Float -> Float -> Texture -> Texture
tileSprite x y spriteSheet =
    let
        paddingLeft =
            40

        paddingTop =
            40

        spriteSize =
            64

        gap =
            32

        vgap =
            32
    in
    Texture.sprite
        { x = x * (spriteSize + gap) + paddingLeft
        , y = y * (spriteSize + vgap) + paddingTop
        , width = spriteSize
        , height = spriteSize
        }
        spriteSheet


tile : Texture -> Tile
tile spriteSheet =
    { soilGrass = tileSprite 0 0 spriteSheet
    , soilSand = tileSprite 1 0 spriteSheet
    , soilIce = tileSprite 2 0 spriteSheet
    , soil = tileSprite 3 0 spriteSheet
    , waterSurface = tileSprite 4 0 spriteSheet
    , lavaSurface = tileSprite 5 0 spriteSheet
    , blueBlock = tileSprite 6 0 spriteSheet
    , yellowBlock = tileSprite 7 0 spriteSheet
    , blueBlockPlaceholder = tileSprite 8 0 spriteSheet
    , yellowBlockPlaceholder = tileSprite 9 0 spriteSheet
    , bluePortal = tileSprite 10 0 spriteSheet
    , yellowPortal = tileSprite 11 0 spriteSheet
    , concreteGrass = tileSprite 0 1 spriteSheet
    , concreteSand = tileSprite 1 1 spriteSheet
    , concreteIce = tileSprite 2 1 spriteSheet
    , concrete = tileSprite 3 1 spriteSheet
    , water = tileSprite 4 1 spriteSheet
    , lava = tileSprite 5 1 spriteSheet
    , greenBlock = tileSprite 6 1 spriteSheet
    , redBlock = tileSprite 7 1 spriteSheet
    , greenBlockPlaceholder = tileSprite 8 1 spriteSheet
    , redBlockPlaceholder = tileSprite 9 1 spriteSheet
    , greenPortal = tileSprite 10 1 spriteSheet
    , redPortal = tileSprite 11 1 spriteSheet
    , soilGrassPlatform = tileSprite 0 2 spriteSheet
    , soilSandPlatform = tileSprite 1 2 spriteSheet
    , soilIcePlatform = tileSprite 2 2 spriteSheet
    , goldBlock = tileSprite 3 2 spriteSheet
    , goldBlockGemHole = tileSprite 4 2 spriteSheet
    , blockedBlockPlaceholder = tileSprite 5 2 spriteSheet
    , gemPlaceholder = tileSprite 6 2 spriteSheet
    , blueGem = tileSprite 7 2 spriteSheet
    , yellowGem = tileSprite 8 2 spriteSheet
    , greenGem = tileSprite 9 2 spriteSheet
    , redGem = tileSprite 10 2 spriteSheet
    , emptyHeart = tileSprite 11 2 spriteSheet
    , concreteGrassPlatform = tileSprite 0 3 spriteSheet
    , concreteSandPlaform = tileSprite 1 3 spriteSheet
    , concreteIcePlatform = tileSprite 2 3 spriteSheet
    , woodBricksBlock = tileSprite 3 3 spriteSheet
    , woodBlock = tileSprite 4 3 spriteSheet
    , allowedBlockPlaceholder = tileSprite 5 3 spriteSheet
    , diamondPlaceholder = tileSprite 6 3 spriteSheet
    , blueDiamond = tileSprite 7 3 spriteSheet
    , yellowDiamond = tileSprite 8 3 spriteSheet
    , greenDiamond = tileSprite 9 3 spriteSheet
    , redDiamond = tileSprite 10 3 spriteSheet
    , halfHeart = tileSprite 11 3 spriteSheet
    , ladderTop = tileSprite 0 4 spriteSheet
    , ladder = tileSprite 1 4 spriteSheet
    , woodFootbridge = tileSprite 2 4 spriteSheet
    , concreteBricksBlock = tileSprite 3 4 spriteSheet
    , concreteBlock = tileSprite 4 4 spriteSheet
    , emptyBlockPlaceholder = tileSprite 5 4 spriteSheet
    , keyPlaceholder = tileSprite 6 4 spriteSheet
    , blueKey = tileSprite 7 4 spriteSheet
    , yellowKey = tileSprite 8 4 spriteSheet
    , greenKey = tileSprite 9 4 spriteSheet
    , redKey = tileSprite 10 4 spriteSheet
    , heart = tileSprite 11 4 spriteSheet
    , spikes = tileSprite 0 5 spriteSheet
    , sawBlade = tileSprite 1 5 spriteSheet
    , grass = tileSprite 2 5 spriteSheet
    , woodFrame = tileSprite 3 5 spriteSheet
    , woodCrate = tileSprite 4 5 spriteSheet
    , openDoorTop = tileSprite 5 5 spriteSheet
    , blueDoorTop = tileSprite 6 5 spriteSheet
    , yellowDoorTop = tileSprite 7 5 spriteSheet
    , greenDoorTop = tileSprite 8 5 spriteSheet
    , redDoorTop = tileSprite 9 5 spriteSheet
    , blueSwitch = tileSprite 10 5 spriteSheet
    , blueSwitchPressed = tileSprite 11 5 spriteSheet
    , yellowSwitch = tileSprite 12 5 spriteSheet
    , yellowSwitchPressed = tileSprite 13 5 spriteSheet
    , openDoor = tileSprite 5 6 spriteSheet
    , blueDoor = tileSprite 6 6 spriteSheet
    , yellowDoor = tileSprite 7 6 spriteSheet
    , greenDoor = tileSprite 8 6 spriteSheet
    , redDoor = tileSprite 9 6 spriteSheet
    , greenSwitch = tileSprite 10 6 spriteSheet
    , greenSwitchPressed = tileSprite 11 6 spriteSheet
    , redSwitch = tileSprite 12 6 spriteSheet
    , redSwitchPressed = tileSprite 13 6 spriteSheet
    }


allTiles =
    [ ( "soilGrass", .soilGrass )
    , ( "soilSand", .soilSand )
    , ( "soilIce", .soilIce )
    , ( "soil", .soil )
    , ( "waterSurface", .waterSurface )
    , ( "lavaSurface", .lavaSurface )
    , ( "blueBlock", .blueBlock )
    , ( "yellowBlock", .yellowBlock )
    , ( "blueBlockPlaceholder", .blueBlockPlaceholder )
    , ( "yellowBlockPlaceholder", .yellowBlockPlaceholder )
    , ( "bluePortal", .bluePortal )
    , ( "yellowPortal", .yellowPortal )
    , ( "concreteGrass", .concreteGrass )
    , ( "concreteSand", .concreteSand )
    , ( "concreteIce", .concreteIce )
    , ( "concrete", .concrete )
    , ( "water", .water )
    , ( "lava", .lava )
    , ( "greenBlock", .greenBlock )
    , ( "redBlock", .redBlock )
    , ( "greenBlockPlaceholder", .greenBlockPlaceholder )
    , ( "redBlockPlaceholder", .redBlockPlaceholder )
    , ( "greenPortal", .greenPortal )
    , ( "redPortal", .redPortal )
    , ( "soilGrassPlatform", .soilGrassPlatform )
    , ( "soilSandPlatform", .soilSandPlatform )
    , ( "soilIcePlatform", .soilIcePlatform )
    , ( "goldBlock", .goldBlock )
    , ( "goldBlockGemHole", .goldBlockGemHole )
    , ( "blockedBlockPlaceholder", .blockedBlockPlaceholder )
    , ( "gemPlaceholder", .gemPlaceholder )
    , ( "blueGem", .blueGem )
    , ( "yellowGem", .yellowGem )
    , ( "greenGem", .greenGem )
    , ( "redGem", .redGem )
    , ( "emptyHeart", .emptyHeart )
    , ( "concreteGrassPlatform", .concreteGrassPlatform )
    , ( "concreteSandPlaform", .concreteSandPlaform )
    , ( "concreteIcePlatform", .concreteIcePlatform )
    , ( "woodBricksBlock", .woodBricksBlock )
    , ( "woodBlock", .woodBlock )
    , ( "allowedBlockPlaceholder", .allowedBlockPlaceholder )
    , ( "diamondPlaceholder", .diamondPlaceholder )
    , ( "blueDiamond", .blueDiamond )
    , ( "yellowDiamond", .yellowDiamond )
    , ( "greenDiamond", .greenDiamond )
    , ( "redDiamond", .redDiamond )
    , ( "halfHeart", .halfHeart )
    , ( "ladderTop", .ladderTop )
    , ( "ladder", .ladder )
    , ( "woodFootbridge", .woodFootbridge )
    , ( "concreteBricksBlock", .concreteBricksBlock )
    , ( "concreteBlock", .concreteBlock )
    , ( "emptyBlockPlaceholder", .emptyBlockPlaceholder )
    , ( "keyPlaceholder", .keyPlaceholder )
    , ( "blueKey", .blueKey )
    , ( "yellowKey", .yellowKey )
    , ( "greenKey", .greenKey )
    , ( "redKey", .redKey )
    , ( "heart", .heart )
    , ( "spikes", .spikes )
    , ( "sawBlade", .sawBlade )
    , ( "grass", .grass )
    , ( "woodFrame", .woodFrame )
    , ( "woodCrate", .woodCrate )
    , ( "openDoorTop", .openDoorTop )
    , ( "blueDoorTop", .blueDoorTop )
    , ( "yellowDoorTop", .yellowDoorTop )
    , ( "greenDoorTop", .greenDoorTop )
    , ( "redDoorTop", .redDoorTop )
    , ( "blueSwitch", .blueSwitch )
    , ( "blueSwitchPressed", .blueSwitchPressed )
    , ( "yellowSwitch", .yellowSwitch )
    , ( "yellowSwitchPressed", .yellowSwitchPressed )
    , ( "openDoor", .openDoor )
    , ( "blueDoor", .blueDoor )
    , ( "yellowDoor", .yellowDoor )
    , ( "greenDoor", .greenDoor )
    , ( "redDoor", .redDoor )
    , ( "greenSwitch", .greenSwitch )
    , ( "greenSwitchPressed", .greenSwitchPressed )
    , ( "redSwitch", .redSwitch )
    , ( "redSwitchPressed", .redSwitchPressed )
    ]
