module Sprites exposing
    ( Char
    , Tiles
    , allTiles
    , char
    , charSpriteSheetDebugRenderable
    , tileSpriteSheetDebugRenderable
    , tiles
    )

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color


type alias Sprite =
    { texture : Texture
    , bx : Float
    , by : Float
    , bw : Float
    , bh : Float
    }


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


type alias Tiles =
    { allowedBlockPlaceholder : Sprite
    , blockedBlockPlaceholder : Sprite
    , blueBlock : Sprite
    , blueBlockPlaceholder : Sprite
    , blueDiamond : Sprite
    , blueDoor : Sprite
    , blueDoorTop : Sprite
    , blueGem : Sprite
    , blueKey : Sprite
    , bluePortal : Sprite
    , blueSwitch : Sprite
    , blueSwitchPressed : Sprite
    , concrete : Sprite
    , concreteBlock : Sprite
    , concreteBricksBlock : Sprite
    , concreteGrass : Sprite
    , concreteGrassPlatform : Sprite
    , concreteIce : Sprite
    , concreteIcePlatform : Sprite
    , concreteSand : Sprite
    , concreteSandPlaform : Sprite
    , diamondPlaceholder : Sprite
    , emptyBlockPlaceholder : Sprite
    , emptyHeart : Sprite
    , gemPlaceholder : Sprite
    , goldBlock : Sprite
    , goldBlockGemHole : Sprite
    , grass : Sprite
    , greenBlock : Sprite
    , greenBlockPlaceholder : Sprite
    , greenDiamond : Sprite
    , greenDoor : Sprite
    , greenDoorTop : Sprite
    , greenGem : Sprite
    , greenKey : Sprite
    , greenPortal : Sprite
    , greenSwitch : Sprite
    , greenSwitchPressed : Sprite
    , halfHeart : Sprite
    , heart : Sprite
    , keyPlaceholder : Sprite
    , ladder : Sprite
    , ladderTop : Sprite
    , lava : Sprite
    , lavaSurface : Sprite
    , openDoor : Sprite
    , openDoorTop : Sprite
    , redBlock : Sprite
    , redBlockPlaceholder : Sprite
    , redDiamond : Sprite
    , redDoor : Sprite
    , redDoorTop : Sprite
    , redGem : Sprite
    , redKey : Sprite
    , redPortal : Sprite
    , redSwitch : Sprite
    , redSwitchPressed : Sprite
    , sawBlade : Sprite
    , soil : Sprite
    , soilGrass : Sprite
    , soilGrassPlatform : Sprite
    , soilIce : Sprite
    , soilIcePlatform : Sprite
    , soilSand : Sprite
    , soilSandPlatform : Sprite
    , spikes : Sprite
    , water : Sprite
    , waterSurface : Sprite
    , woodBlock : Sprite
    , woodBricksBlock : Sprite
    , woodCrate : Sprite
    , woodFootbridge : Sprite
    , woodFrame : Sprite
    , yellowBlock : Sprite
    , yellowBlockPlaceholder : Sprite
    , yellowDiamond : Sprite
    , yellowDoor : Sprite
    , yellowDoorTop : Sprite
    , yellowGem : Sprite
    , yellowKey : Sprite
    , yellowPortal : Sprite
    , yellowSwitch : Sprite
    , yellowSwitchPressed : Sprite
    }


tileSpriteSheetDebugRenderable : Texture -> List Renderable
tileSpriteSheetDebugRenderable tileSpriteSheet =
    let
        ts =
            tiles tileSpriteSheet
    in
    allTiles
        |> List.indexedMap
            (\i ( name, git ) ->
                let
                    tile =
                        git ts

                    d =
                        Texture.dimensions tile.texture

                    x =
                        toFloat (i |> modBy 14) * (d.width + 10) + 10

                    y =
                        toFloat (i // 14) * (d.height + 10) + 10
                in
                [ texture [] ( x, y ) tile.texture
                , shapes [ alpha 0.3, stroke Color.red, fill Color.lightRed ]
                    [ rect
                        ( x + tile.bx
                        , y + tile.by
                        )
                        tile.bw
                        tile.bh
                    ]
                ]
            )
        |> List.concat


tileSprite : Float -> Float -> Float -> Float -> Float -> Float -> Texture -> Sprite
tileSprite x y bx by bwd bhd spriteSheet =
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
    { texture =
        Texture.sprite
            { x = x * (spriteSize + gap) + paddingLeft
            , y = y * (spriteSize + vgap) + paddingTop
            , width = spriteSize
            , height = spriteSize
            }
            spriteSheet
    , bx = bx
    , by = by
    , bw = spriteSize - bwd
    , bh = spriteSize - bhd
    }


tiles : Texture -> Tiles
tiles spriteSheet =
    { soilGrass = tileSprite 0 0 0 0 0 0 spriteSheet
    , soilSand = tileSprite 1 0 0 0 0 0 spriteSheet
    , soilIce = tileSprite 2 0 0 0 0 0 spriteSheet
    , soil = tileSprite 3 0 0 0 0 0 spriteSheet
    , waterSurface = tileSprite 4 0 0 9 0 9 spriteSheet
    , lavaSurface = tileSprite 5 0 0 9 0 9 spriteSheet
    , blueBlock = tileSprite 6 0 0 0 0 0 spriteSheet
    , yellowBlock = tileSprite 7 0 0 0 0 0 spriteSheet
    , blueBlockPlaceholder = tileSprite 8 0 0 0 0 0 spriteSheet
    , yellowBlockPlaceholder = tileSprite 9 0 0 0 0 0 spriteSheet
    , bluePortal = tileSprite 10 0 13 13 26 26 spriteSheet
    , yellowPortal = tileSprite 11 0 13 13 26 26 spriteSheet
    , concreteGrass = tileSprite 0 1 0 0 0 0 spriteSheet
    , concreteSand = tileSprite 1 1 0 0 0 0 spriteSheet
    , concreteIce = tileSprite 2 1 0 0 0 0 spriteSheet
    , concrete = tileSprite 3 1 0 0 0 0 spriteSheet
    , water = tileSprite 4 1 0 0 0 0 spriteSheet
    , lava = tileSprite 5 1 0 0 0 0 spriteSheet
    , greenBlock = tileSprite 6 1 0 0 0 0 spriteSheet
    , redBlock = tileSprite 7 1 0 0 0 0 spriteSheet
    , greenBlockPlaceholder = tileSprite 8 1 0 0 0 0 spriteSheet
    , redBlockPlaceholder = tileSprite 9 1 0 0 0 0 spriteSheet
    , greenPortal = tileSprite 10 1 13 13 26 26 spriteSheet
    , redPortal = tileSprite 11 1 13 13 26 26 spriteSheet
    , soilGrassPlatform = tileSprite 0 2 0 0 0 29 spriteSheet
    , soilSandPlatform = tileSprite 1 2 0 0 0 29 spriteSheet
    , soilIcePlatform = tileSprite 2 2 0 0 0 29 spriteSheet
    , goldBlock = tileSprite 3 2 0 0 0 0 spriteSheet
    , goldBlockGemHole = tileSprite 4 2 0 0 0 0 spriteSheet
    , blockedBlockPlaceholder = tileSprite 5 2 0 0 0 0 spriteSheet
    , gemPlaceholder = tileSprite 6 2 22 22 44 44 spriteSheet
    , blueGem = tileSprite 7 2 22 22 44 44 spriteSheet
    , yellowGem = tileSprite 8 2 22 22 44 44 spriteSheet
    , greenGem = tileSprite 9 2 22 22 44 44 spriteSheet
    , redGem = tileSprite 10 2 22 22 44 44 spriteSheet
    , emptyHeart = tileSprite 11 2 20 22 40 46 spriteSheet
    , concreteGrassPlatform = tileSprite 0 3 0 0 0 29 spriteSheet
    , concreteSandPlaform = tileSprite 1 3 0 0 0 29 spriteSheet
    , concreteIcePlatform = tileSprite 2 3 0 0 0 29 spriteSheet
    , woodBricksBlock = tileSprite 3 3 0 0 0 0 spriteSheet
    , woodBlock = tileSprite 4 3 0 0 0 0 spriteSheet
    , allowedBlockPlaceholder = tileSprite 5 3 0 0 0 0 spriteSheet
    , diamondPlaceholder = tileSprite 6 3 20 22 38 46 spriteSheet
    , blueDiamond = tileSprite 7 3 20 22 38 46 spriteSheet
    , yellowDiamond = tileSprite 8 3 20 22 38 46 spriteSheet
    , greenDiamond = tileSprite 9 3 20 22 38 46 spriteSheet
    , redDiamond = tileSprite 10 3 20 22 38 46 spriteSheet
    , halfHeart = tileSprite 11 3 20 22 40 46 spriteSheet
    , ladderTop = tileSprite 0 4 0 0 0 0 spriteSheet
    , ladder = tileSprite 1 4 0 0 0 0 spriteSheet
    , woodFootbridge = tileSprite 2 4 0 0 0 33 spriteSheet
    , concreteBricksBlock = tileSprite 3 4 0 0 0 0 spriteSheet
    , concreteBlock = tileSprite 4 4 0 0 0 0 spriteSheet
    , emptyBlockPlaceholder = tileSprite 5 4 0 0 0 0 spriteSheet
    , keyPlaceholder = tileSprite 6 4 12 21 25 40 spriteSheet
    , blueKey = tileSprite 7 4 12 21 25 40 spriteSheet
    , yellowKey = tileSprite 8 4 12 21 25 40 spriteSheet
    , greenKey = tileSprite 9 4 12 21 25 40 spriteSheet
    , redKey = tileSprite 10 4 12 21 25 40 spriteSheet
    , heart = tileSprite 11 4 20 22 40 46 spriteSheet
    , spikes = tileSprite 0 5 0 32 0 32 spriteSheet
    , sawBlade = tileSprite 1 5 6 6 12 12 spriteSheet
    , grass = tileSprite 2 5 0 0 0 0 spriteSheet
    , woodFrame = tileSprite 3 5 0 0 0 0 spriteSheet
    , woodCrate = tileSprite 4 5 0 0 0 0 spriteSheet
    , openDoorTop = tileSprite 5 5 10 10 20 10 spriteSheet
    , blueDoorTop = tileSprite 6 5 10 10 20 10 spriteSheet
    , yellowDoorTop = tileSprite 7 5 10 10 20 10 spriteSheet
    , greenDoorTop = tileSprite 8 5 10 10 20 10 spriteSheet
    , redDoorTop = tileSprite 9 5 10 10 20 10 spriteSheet
    , blueSwitch = tileSprite 10 5 0 0 0 0 spriteSheet
    , blueSwitchPressed = tileSprite 11 5 0 0 0 0 spriteSheet
    , yellowSwitch = tileSprite 12 5 0 0 0 0 spriteSheet
    , yellowSwitchPressed = tileSprite 13 5 0 0 0 0 spriteSheet
    , openDoor = tileSprite 5 6 10 0 20 0 spriteSheet
    , blueDoor = tileSprite 6 6 10 0 20 0 spriteSheet
    , yellowDoor = tileSprite 7 6 10 0 20 0 spriteSheet
    , greenDoor = tileSprite 8 6 10 0 20 0 spriteSheet
    , redDoor = tileSprite 9 6 10 0 20 0 spriteSheet
    , greenSwitch = tileSprite 10 6 10 33 20 33 spriteSheet
    , greenSwitchPressed = tileSprite 11 6 10 44 20 44 spriteSheet
    , redSwitch = tileSprite 12 6 10 33 20 33 spriteSheet
    , redSwitchPressed = tileSprite 13 6 10 44 20 44 spriteSheet
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
