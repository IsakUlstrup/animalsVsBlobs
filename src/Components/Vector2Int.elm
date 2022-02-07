module Components.Vector2Int exposing (directionTo, distanceTo, get, length, negate, negateX, negateY, newVector2, normalize, scale, setX, setY)

{-| A vector type that stores values as ints multiplied by 1000, should lead to precise math?
-}

import Components.DecimalFloat exposing (DecimalFloat, add, div, getDecFloat, multi, newDecFloat, sqrtDF)


type alias Vector2Int =
    { x : DecimalFloat
    , y : DecimalFloat
    }


newVector2 : Float -> Float -> Vector2Int
newVector2 x y =
    Vector2Int (newDecFloat x 3) (newDecFloat y 3)


get : Vector2Int -> ( Float, Float )
get vec =
    ( vec.x |> getDecFloat
    , vec.y |> getDecFloat
    )


scale : Float -> Vector2Int -> Vector2Int
scale s vec =
    { vec
        | x = multi (newDecFloat s 3) vec.x
        , y = multi (newDecFloat s 3) vec.y
    }


setX : Float -> Vector2Int -> Vector2Int
setX x vec =
    { vec | x = newDecFloat x 3 }


setY : Float -> Vector2Int -> Vector2Int
setY y vec =
    { vec | y = newDecFloat y 3 }


negate : Vector2Int -> Vector2Int
negate vec =
    vec |> negateX |> negateY


negateX : Vector2Int -> Vector2Int
negateX vec =
    { vec
        | x = multi vec.x (newDecFloat -1 3)
    }


negateY : Vector2Int -> Vector2Int
negateY vec =
    { vec
        | y = multi vec.y (newDecFloat -1 3)
    }



-- multi : Vector2 -> Vector2 -> Vector2
-- multi v1 v2 =
--     { v1 | x = v1.x * v2.x, y = v1.y * v2.y }
-- add : Vector2 -> Vector2 -> Vector2
-- add v1 v2 =
--     { v1 | x = v1.x + v2.x, y = v1.y + v2.y }
-- sub : Vector2 -> Vector2 -> Vector2
-- sub v1 v2 =
--     { v1 | x = v1.x - v2.x, y = v1.y - v2.y }
-- scale : Float -> Vector2Int -> Vector2Int
-- scale amount vector =
--     { vector | x = multi vector.x (newDecFloat amount 3), y = multi vector.y (newDecFloat amount 3) }


distanceTo : Vector2Int -> Vector2Int -> Float
distanceTo v1 v2 =
    let
        a =
            Components.DecimalFloat.sub v1.x v2.x

        b =
            Components.DecimalFloat.sub v1.y v2.y
    in
    sqrtDF (add (multi a a) (multi b b)) |> getDecFloat


length : Vector2Int -> DecimalFloat
length vec =
    let
        a =
            multi vec.x vec.x

        b =
            multi vec.y vec.y
    in
    sqrtDF (add a b)


normalize : Vector2Int -> Vector2Int
normalize vec =
    { vec
        | x = div vec.x (length vec)
        , y = div vec.y (length vec)
    }


{-| Returns a normalized vector pointing from origin to target
-}
directionTo : Vector2Int -> Vector2Int -> Vector2Int
directionTo origin target =
    { x = Components.DecimalFloat.sub target.x origin.x, y = Components.DecimalFloat.sub target.y origin.y }
        |> normalize
