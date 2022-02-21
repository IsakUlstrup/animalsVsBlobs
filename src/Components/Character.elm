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


update : Float -> Character -> Character
update dt character =
    let
        targetDist =
            case character.state of
                Idle ->
                    0

                AiMove trgt ->
                    Vector2.distance character.position trgt

                ManualMove trgt ->
                    Vector2.distance character.position trgt

        target =
            case character.state of
                Idle ->
                    Nothing

                AiMove trgt ->
                    Just trgt

                ManualMove trgt ->
                    Just trgt
    in
    -- { character
    --     | acceleration = Vector2.scale friction character.acceleration
    --     , position =
    --         character.position
    --             |> Vector2.add (Vector2.scale (dt * character.movementSpeedModifier) character.velocity)
    --             |> Vector2.add (Vector2.scale (dt * 0.1) character.acceleration)
    -- }
    --     |> constrainPosition
    if targetDist > 1.5 then
        case target of
            Nothing ->
                { character
                    | velocity = Vector2.add character.velocity character.acceleration
                    , position = Vector2.add character.position (Vector2.scale dt character.velocity)
                }

            Just t ->
                { character
                    | velocity = Vector2.direction character.position t |> Vector2.scale character.movementSpeedModifier
                    , position = Vector2.add character.position (Vector2.scale dt character.velocity)
                }

    else
        { character | velocity = Vector2.setMagnitude 0 character.velocity, state = Idle }


constrainLeft : Character -> Character
constrainLeft character =
    if character.position.x < -50 + character.radius then
        { character
            | position = Vector2.setX (-50 + character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.5 |> Vector2.negateX

            -- , velocity = Vector2.negateX character.velocity
            , velocity = Vector2.setX 0 character.velocity
        }

    else
        character


constrainRight : Character -> Character
constrainRight character =
    if character.position.x > 50 - character.radius then
        { character
            | position = Vector2.setX (50 - character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.5 |> Vector2.negateX

            -- , velocity = Vector2.negateX character.velocity
            , velocity = Vector2.setX 0 character.velocity
        }

    else
        character


constrainPosition : Character -> Character
constrainPosition character =
    -- TODO: handle a character being out of bounds in two dimensions at the same time
    -- left
    if character.position.x < -50 + character.radius then
        { character
            | position = Vector2.setX (-50 + character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.5 |> Vector2.negateX

            -- , velocity = Vector2.negateX character.velocity
            , velocity = Vector2.setX 0 character.velocity
        }
        -- right

    else if character.position.x > 50 - character.radius then
        { character
            | position = Vector2.setX (50 - character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.5 |> Vector2.negateX

            -- , velocity = Vector2.negateX character.velocity
            , velocity = Vector2.setX 0 character.velocity
        }
        -- top

    else if character.position.y < -50 + character.radius then
        { character
            | position = Vector2.setY (-50 + character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.5 |> Vector2.negateY

            -- , velocity = Vector2.negateY character.velocity
            , velocity = Vector2.setY 0 character.velocity
        }
        -- bottom

    else if character.position.y > 50 - character.radius then
        { character
            | position = Vector2.setY (50 - character.radius) character.position
            , acceleration = character.acceleration |> Vector2.scale 0.5 |> Vector2.negateY

            -- , velocity = Vector2.negateY character.velocity
            , velocity = Vector2.setY 0 character.velocity
        }

    else
        character


setMoveTarget : Character -> Character -> Character
setMoveTarget target character =
    { character | velocity = direction character.position target.position }


setMoveTargetVector : Vector2 -> Character -> Character
setMoveTargetVector target character =
    { character | velocity = direction character.position target, state = ManualMove target }


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
            { char | velocity = new 0 0 }


keepEnemyDistance : Float -> List Character -> Character -> Character
keepEnemyDistance safeDistance characters char =
    case closestEnemy char characters of
        Just target ->
            let
                dist =
                    Vector2.distance char.position target.position
            in
            if dist < safeDistance then
                -- Enemy is too close, move away
                { char | velocity = direction char.position target.position |> Vector2.scale (1 - (dist / safeDistance)) |> Vector2.negate }

            else
                { char | velocity = new 0 0 }

        Nothing ->
            { char | velocity = new 0 0 }


aiMove : List Character -> Character -> Character
aiMove characters char =
    if char.aggressive then
        moveTowardsClosestEnemy characters char

    else
        keepEnemyDistance 25 characters char


isColliding : Character -> Character -> Bool
isColliding c1 c2 =
    distanceBetween c1 c2 < c1.radius + c2.radius


collision : List Character -> Character -> Character
collision chars char =
    List.foldl
        (\c1 c2 ->
            if c1 /= c2 then
                -- only check collision betweeen enemies, ignore self
                if isColliding c1 c2 then
                    let
                        shift =
                            Vector2.subtract c2.position c1.position
                                |> Vector2.scale 0.01
                    in
                    -- resolve collision
                    { c2
                        | position = Vector2.add c2.position shift

                        -- , radius = c2.radius - 0.05
                    }
                    -- |> applyForce (Vector2.scale (c1.radius * 0.7) shift)

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
