module Components.Character exposing (..)

import Components.Vector2 as Vector2 exposing (Vector2, directionTo, newVector2)


type alias Character =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , player : Bool
    , radius : Float
    , movementSpeedModifier : Float
    }


newCharacter : ( Float, Float ) -> Bool -> Float -> Float -> Character
newCharacter ( x, y ) player size speed =
    Character (newVector2 x y) (newVector2 0 0) (newVector2 0 0) player size speed


distanceBetween : Character -> Character -> Float
distanceBetween c1 c2 =
    Vector2.distanceTo c1.position c2.position


update : Float -> Character -> Character
update dt character =
    let
        friction =
            1 - (dt * 0.005)
    in
    { character
        | acceleration = Vector2.scale friction character.acceleration
        , position =
            character.position
                |> Vector2.add (Vector2.scale (dt * character.movementSpeedModifier) (Vector2.add character.velocity character.acceleration))
    }
        |> constrainPosition


constrainPosition : Character -> Character
constrainPosition character =
    if character.position.x < -50 + character.radius then
        { character
            | position = Vector2.setX (-50 + character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.9 |> Vector2.negateX
            , velocity = Vector2.negateX character.velocity
        }

    else if character.position.x > 50 - character.radius then
        { character
            | position = Vector2.setX (50 - character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.9 |> Vector2.negateX
            , velocity = Vector2.negateX character.velocity
        }

    else if character.position.y < -50 + character.radius then
        { character
            | position = Vector2.setY (-50 + character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.9 |> Vector2.negateY
            , velocity = Vector2.negateY character.velocity
        }

    else if character.position.y > 50 - character.radius then
        { character
            | position = Vector2.setY (50 - character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.9 |> Vector2.negateY
            , velocity = Vector2.negateY character.velocity
        }

    else
        character


setMoveTarget : Character -> Character -> Character
setMoveTarget target character =
    { character | velocity = directionTo character.position target.position }


applyForce : Vector2 -> Character -> Character
applyForce force character =
    { character | acceleration = Vector2.add character.acceleration (Vector2.scale (1 / character.radius) force) }


closestEnemy : Character -> List Character -> Maybe Character
closestEnemy char characters =
    let
        isEnemy : Character -> Bool
        isEnemy c =
            c.player /= char.player

        closestAcc : Character -> Maybe Character -> Maybe Character
        closestAcc c closest =
            case closest of
                Just cls ->
                    if distanceBetween char c < distanceBetween char cls then
                        Just c

                    else
                        closest

                Nothing ->
                    Just c
    in
    List.filter isEnemy characters
        |> List.foldl closestAcc Nothing


moveTowardsClosestEnemy : List Character -> Character -> Character
moveTowardsClosestEnemy characters char =
    case closestEnemy char characters of
        Just target ->
            setMoveTarget target char

        Nothing ->
            { char | velocity = newVector2 0 0 }


isColliding : Character -> Character -> Bool
isColliding c1 c2 =
    distanceBetween c1 c2 < c1.radius + c2.radius


collision : List Character -> Character -> Character
collision chars char =
    List.foldl
        (\c1 c2 ->
            if c1.player /= c2.player && c1 /= c2 then
                -- only check collision betweeen enemies, ignore self
                if isColliding c1 c2 then
                    let
                        shift =
                            Vector2.sub c2.position c1.position
                                |> Vector2.normalize
                    in
                    -- resolve collision
                    { c2
                        | position = Vector2.add c2.position shift
                        , radius = c2.radius - 0.2
                    }
                        |> applyForce (Vector2.scale (c1.radius * 5) shift)

                else
                    c2

            else
                c2
        )
        char
        chars


isAlive : Character -> Bool
isAlive character =
    character.radius >= 1
