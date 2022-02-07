module Components.Vector2 exposing
    ( Vector2
    , add
    , directionTo
    , distanceTo
    , multi
    , negate
    , negateX
    , negateY
    , newVector2
    , normalize
    , scale
    , setX
    , setY
    , sub
    )


type alias Vector2 =
    { x : Float
    , y : Float
    }


newVector2 : Float -> Float -> Vector2
newVector2 x y =
    Vector2 x y


setX : Float -> Vector2 -> Vector2
setX x vector =
    { vector | x = x }


setY : Float -> Vector2 -> Vector2
setY y vector =
    { vector | y = y }


negate : Vector2 -> Vector2
negate vector =
    { vector
        | x = vector.x * -1
        , y = vector.y * -1
    }


negateX : Vector2 -> Vector2
negateX vector =
    { vector
        | x = vector.x * -1
    }


negateY : Vector2 -> Vector2
negateY vector =
    { vector
        | y = vector.x * -1
    }


multi : Vector2 -> Vector2 -> Vector2
multi v1 v2 =
    { v1 | x = v1.x * v2.x, y = v1.y * v2.y }


add : Vector2 -> Vector2 -> Vector2
add v1 v2 =
    { v1 | x = v1.x + v2.x, y = v1.y + v2.y }


sub : Vector2 -> Vector2 -> Vector2
sub v1 v2 =
    { v1 | x = v1.x - v2.x, y = v1.y - v2.y }


scale : Float -> Vector2 -> Vector2
scale amount vector =
    { vector | x = vector.x * amount, y = vector.y * amount }


distanceTo : Vector2 -> Vector2 -> Float
distanceTo v1 v2 =
    let
        a =
            v1.x - v2.x

        b =
            v1.y - v2.y
    in
    sqrt (a * a + b * b)


length : Vector2 -> Float
length vector =
    sqrt (vector.x * vector.x + vector.y * vector.y)


normalize : Vector2 -> Vector2
normalize vector =
    if vector.x == 0 && vector.y == 0 then
        newVector2 0 0

    else
        { vector
            | x = vector.x / length vector
            , y = vector.y / length vector
        }


{-| Returns a normalized vector pointing from origin to target
-}
directionTo : Vector2 -> Vector2 -> Vector2
directionTo origin target =
    Vector2 (target.x - origin.x) (target.y - origin.y) |> normalize
