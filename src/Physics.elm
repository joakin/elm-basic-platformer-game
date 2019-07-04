module Physics exposing
    ( CollisionType(..)
    , Object
    , collisionCheck
    , emptyObject
    , integrate
    , push
    , standingOn
    )


type alias Object =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , bx : Float
    , by : Float
    , bw : Float
    , bh : Float
    , vx : Float
    , vy : Float
    , ax : Float
    , ay : Float
    }


emptyObject : Object
emptyObject =
    { x = 0
    , y = 0
    , w = 0
    , h = 0
    , bx = 0
    , by = 0
    , bw = 0
    , bh = 0
    , vx = 0
    , vy = 0
    , ax = 0
    , ay = 0
    }


push : Float -> Float -> Object -> Object
push ax ay obj =
    { obj | ax = ax, ay = ay }


type CollisionType
    = L
    | R
    | T
    | B


collisionCheck : Object -> Object -> ( Maybe CollisionType, Object )
collisionCheck o1 o2 =
    -- get the vectors to check against
    let
        vX =
            (o1.x + o1.bx + (o1.bw / 2)) - (o2.x + o2.bx + (o2.bw / 2))

        vY =
            (o1.y + o1.by + (o1.bh / 2)) - (o2.y + o2.by + (o2.bh / 2))

        -- add the half widths and half heights of the objects
        hWidths =
            (o1.bw / 2) + (o2.bw / 2)

        hHeights =
            (o1.bh / 2) + (o2.bh / 2)

        colDir =
            Nothing
    in
    -- if the x and y vector are less than the half width or half height, they
    -- we must be inside the object, causing a collision
    if abs vX < hWidths && abs vY < hHeights then
        -- figures out on which side we are colliding (top, bottom, left, or right)
        let
            oX =
                hWidths - abs vX

            oY =
                hHeights - abs vY
        in
        if oX >= oY then
            if vY > 0 then
                ( Just T, { o1 | y = o1.y + oY } )

            else
                ( Just B, { o1 | y = o1.y - oY } )

        else if vX > 0 then
            ( Just L, { o1 | x = o1.x + oX } )

        else
            ( Just R, { o1 | x = o1.x - oX } )

    else
        ( Nothing, o1 )


standingOn : Object -> Object -> Bool
standingOn o2 o1 =
    if
        (o1.x + o1.bx + o1.bw > o2.x + o2.bx)
            && (o1.x + o1.bx < o2.x + o2.bx + o2.bw)
            && (o1.y + o1.by + o1.bh >= o2.y + o2.by)
            && (o1.y + o1.by < o2.y + o2.by + o2.bh)
    then
        True

    else
        False


integrate : Float -> Float -> Float -> Object -> Object
integrate frictionX frictionY gravity obj =
    { obj
        | x = obj.x + obj.vx
        , y = obj.y + obj.vy
        , vx = obj.vx * frictionX + obj.ax
        , vy = obj.vy * frictionY + obj.ay + gravity
        , ax = 0
        , ay = 0
    }
