module Components.DecimalFloat exposing (DecimalFloat, add, div, getDecFloat, multi, newDecFloat, sqrtDF, sub)


type DecimalFloat
    = DecimalFloat Int Int


{-| Create a new decimal float.

number is any floating point number, dec is the number of decimals

valid decimal number range is 0 - 5, values outside range will ble clamped

-}
newDecFloat : Float -> Int -> DecimalFloat
newDecFloat number decimals =
    let
        dec =
            10 ^ clamp 0 5 decimals
    in
    DecimalFloat (number * toFloat dec |> round) dec


getDecFloat : DecimalFloat -> Float
getDecFloat (DecimalFloat n d) =
    (n |> toFloat) / toFloat d


{-| Add two decimal floats together, only works for numbers with the same decimal count for now

if the two numbers have different decimal count, return the second

-}
add : DecimalFloat -> DecimalFloat -> DecimalFloat
add (DecimalFloat n1 d1) (DecimalFloat n2 d2) =
    if d1 == d2 then
        DecimalFloat (n1 + n2) d2

    else
        DecimalFloat n2 d2


sub : DecimalFloat -> DecimalFloat -> DecimalFloat
sub (DecimalFloat n1 d1) (DecimalFloat n2 d2) =
    if d1 == d2 then
        DecimalFloat (n1 - n2) d2

    else
        DecimalFloat n2 d2


multi : DecimalFloat -> DecimalFloat -> DecimalFloat
multi (DecimalFloat n1 d1) (DecimalFloat n2 d2) =
    if d1 == d2 then
        DecimalFloat (n1 * n2 // d2) d2

    else
        DecimalFloat n2 d2


div : DecimalFloat -> DecimalFloat -> DecimalFloat
div (DecimalFloat n1 d1) (DecimalFloat n2 d2) =
    if d1 == d2 then
        if n2 == 0 then
            DecimalFloat 0 d2

        else
            newDecFloat (toFloat n1 / toFloat n2) d2

    else
        DecimalFloat n2 d2


sqrtDF : DecimalFloat -> DecimalFloat
sqrtDF ((DecimalFloat _ d) as num) =
    newDecFloat (sqrt (getDecFloat num)) d
