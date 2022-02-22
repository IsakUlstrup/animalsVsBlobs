module Components.Character exposing (..)

import Components.Vector2 as Vector2 exposing (Vector2, direction, new)


type CharacterState
    = Idle
    | AiMove Vector2
    | ManualMove Vector2


type alias Character =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , player : Bool
    , radius : Float
    , movementSpeedModifier : Float
    , appearance : Maybe String
    , aggressive : Bool
    , state : CharacterState
    }


newCharacter : ( Float, Float ) -> Bool -> Float -> Float -> Maybe String -> Character
newCharacter ( x, y ) player size speed app =
    Character (new x y) (new 0 0) (new 0 0) player size speed app True Idle


newPassiveCharacter : ( Float, Float ) -> Bool -> Float -> Float -> Maybe String -> Character
newPassiveCharacter ( x, y ) player size speed app =
    Character (new x y) (new 0 0) (new 0 0) player size speed app False Idle


distanceBetween : Character -> Character -> Float
distanceBetween c1 c2 =
    Vector2.distance c1.position c2.position


aiForce : Vector2 -> Character -> Character
aiForce target character =
    let
        targetDist =
            Vector2.distance character.position target
    in
    if targetDist < 20 then
        { character | state = Idle }

    else
        character
            |> applyForce
                (Vector2.direction character.position target
                    |> Vector2.scale (targetDist * 0.001)
                )


{-| apply forces based on character target
-}
aiUpdate : Character -> Character
aiUpdate character =
    case character.state of
        Idle ->
            character

        ManualMove t ->
            aiForce t character

        AiMove t ->
            aiForce t character


{-| move character based on acceleration and velocity
-}
update : Float -> Character -> Character
update dt character =
    { character
        | velocity =
            Vector2.add character.velocity character.acceleration
                |> Vector2.limitMagnitude character.movementSpeedModifier
                |> Vector2.scale 0.97
        , position = Vector2.add character.position (Vector2.scale dt character.velocity)
        , acceleration = Vector2.setMagnitude 0 character.acceleration
    }


aiSetMoveTarget : Vector2 -> Character -> Character
aiSetMoveTarget target character =
    case character.state of
        Idle ->
            { character | velocity = direction character.position target, state = AiMove target }

        ManualMove _ ->
            character

        AiMove _ ->
            { character | velocity = direction character.position target, state = AiMove target }


setMoveTargetVector : Vector2 -> Character -> Character
setMoveTargetVector target character =
    { character | state = ManualMove target }


applyForce : Vector2 -> Character -> Character
applyForce force character =
    { character | acceleration = Vector2.add character.acceleration force }


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


keepEnemyDistance : Float -> List Character -> Character -> Maybe Vector2
keepEnemyDistance safeDistance characters char =
    closestEnemy char characters
        |> Maybe.andThen
            (\target ->
                let
                    dist =
                        Vector2.distance char.position target.position
                in
                if dist < safeDistance then
                    Just (Vector2.add char.position (Vector2.direction target.position char.position |> Vector2.setMagnitude 50))

                else
                    Nothing
            )


{-| Set movement target for a character
-}
aiMove : List Character -> Character -> Character
aiMove characters char =
    case char.state of
        ManualMove _ ->
            char

        _ ->
            if char.aggressive then
                case closestEnemy char characters of
                    Just t ->
                        { char | state = AiMove t.position }

                    Nothing ->
                        char
                -- moveTowardsClosestEnemy characters char

            else
                -- keepEnemyDistance 500 characters char
                case keepEnemyDistance 100 characters char of
                    Just t ->
                        { char | state = AiMove t }

                    Nothing ->
                        { char | state = Idle }


setIdle : Character -> Character
setIdle character =
    { character | state = Idle }


isColliding : Character -> Character -> Bool
isColliding c1 c2 =
    distanceBetween c1 c2 < c1.radius + c2.radius


collision : List Character -> Character -> Character
collision chars char =
    List.foldl
        (\c1 c2 ->
            if c1 /= c2 && isColliding c1 c2 then
                -- ignore self
                let
                    d =
                        Vector2.distance c1.position c2.position - (c1.radius + c2.radius)

                    shift =
                        Vector2.subtract c2.position c1.position
                            |> Vector2.scale 0.001
                            |> Vector2.scale (d * -1)
                in
                c2
                    |> applyForce shift

            else
                c2
        )
        char
        chars


isAlive : Character -> Bool
isAlive character =
    character.radius >= 1
