module Components.Character exposing (..)

import Components.Range exposing (Range, newRange)
import Components.Vector2 as Vector2 exposing (Vector2, direction, new)


type CharacterState
    = Idle
    | AiMove Vector2
    | ManualMove Vector2


type alias Skill =
    { cooldown : Float
    }


type alias Character =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , player : Bool
    , radius : Float
    , movementSpeedModifier : Float
    , appearance : Maybe String

    -- , aggressive : Bool
    , state : CharacterState
    , skill : Maybe Skill
    , preferredDistance : Float
    , visionRange : Float
    , damage : Float
    , health : Range Float
    }



---- CHARACTER BUILDER ----


init : ( Float, Float ) -> Character
init ( x, y ) =
    Character (new x y) (new 0 0) (new 0 0) False 15 1 Nothing Idle Nothing 100 500 1 (newRange 0 100 100)


{-| Set wether character is a player or not
-}
withPlayerFlag : Bool -> Character -> Character
withPlayerFlag flag char =
    { char | player = flag }


withPreferredDistance : Float -> Character -> Character
withPreferredDistance dist char =
    { char | preferredDistance = dist }


withVisionRange : Float -> Character -> Character
withVisionRange dist char =
    { char | visionRange = dist }


withMoveSpeed : Float -> Character -> Character
withMoveSpeed speed char =
    { char | movementSpeedModifier = speed }


withAppearance : Maybe String -> Character -> Character
withAppearance app char =
    { char | appearance = app }


withHealth : Range Float -> Character -> Character
withHealth health char =
    { char | health = health }


withSkill : Maybe Skill -> Character -> Character
withSkill skill char =
    { char | skill = skill }


withDamage : Float -> Character -> Character
withDamage dmg char =
    { char | damage = dmg }



---- SKILL ----


testSkill : Skill
testSkill =
    Skill 100


updateSkill : Float -> Skill -> Skill
updateSkill dt skill =
    { skill | cooldown = skill.cooldown - dt }


useSkill : Character -> Character
useSkill char =
    case char.skill of
        Nothing ->
            char

        Just _ ->
            { char | skill = Just testSkill }


skillIsReady : Character -> Bool
skillIsReady char =
    case char.skill of
        Just s ->
            s.cooldown <= 0

        Nothing ->
            False


updateCharacterSkill : Float -> Character -> Character
updateCharacterSkill dt char =
    case char.skill of
        Nothing ->
            char

        Just s ->
            { char | skill = Just (updateSkill dt s) }



---- MAIN LOGIC ----


aiForce : Vector2 -> Character -> Character
aiForce target character =
    let
        targetDist =
            Vector2.distance character.position target
    in
    if targetDist < (character.radius * 1.5) then
        { character | state = Idle }

    else
        character |> applyForce (Vector2.direction character.position target)


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
    let
        friction =
            1 - (dt * 0.095)
    in
    { character
        | velocity =
            Vector2.add character.velocity (Vector2.scale dt character.acceleration)
                |> Vector2.scale friction
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
    { character | acceleration = Vector2.add character.acceleration (Vector2.scale character.movementSpeedModifier force) }


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
                    if Vector2.distance char.position c.position < Vector2.distance char.position cls.position then
                        Just c

                    else
                        closest

                Nothing ->
                    Just c
    in
    List.filter isEnemy characters
        |> List.foldl closestAcc Nothing


keepDistance : List Character -> Character -> Maybe Vector2
keepDistance characters char =
    closestEnemy char characters
        |> Maybe.andThen
            (\target ->
                let
                    dist =
                        Vector2.distance char.position target.position
                in
                -- target is too close, move away
                if dist < char.preferredDistance && isIdle char then
                    Just (Vector2.add char.position (Vector2.direction target.position char.position |> Vector2.setMagnitude 100))
                    -- target is too far, move closer

                else if dist > char.preferredDistance && dist < char.visionRange && isIdle char then
                    Just (Vector2.add char.position (Vector2.direction char.position target.position |> Vector2.setMagnitude 100))

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
            case keepDistance characters char of
                Just t ->
                    { char | state = AiMove t }

                Nothing ->
                    { char | state = Idle }


setIdle : Character -> Character
setIdle character =
    { character | state = Idle }


isColliding : Character -> Character -> Bool
isColliding c1 c2 =
    Vector2.distance c1.position c2.position < c1.radius + c2.radius


areEnemies : Character -> Character -> Bool
areEnemies c1 c2 =
    c1.player /= c2.player


isIdle : Character -> Bool
isIdle char =
    case char.state of
        Idle ->
            True

        _ ->
            False


isAlive : Character -> Bool
isAlive char =
    Components.Range.isEmpty char.health |> not


isWithinVision : Character -> Character -> Bool
isWithinVision char target =
    Vector2.distance char.position target.position <= char.visionRange && areEnemies char target


takeDamage : Float -> Character -> Character
takeDamage dmg char =
    { char | health = Components.Range.subtract dmg char.health }


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
                            |> Vector2.scale 0.01
                            |> Vector2.scale (d * -1)
                in
                if areEnemies c1 c2 then
                    c2
                        |> applyForce shift
                        |> takeDamage c1.damage

                else
                    c2
                        |> applyForce shift

            else
                c2
        )
        char
        chars
