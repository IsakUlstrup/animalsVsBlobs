module Components.Skill exposing
    ( Skill
    , SkillEffect(..)
    , buffEffect
    , damageEffect
    , isReady
    , newSkill
    , reduceCooldown
    , resetCooldown
    , setTarget
    , unsetTarget
    )

import Components.Range exposing (Range, isEmpty, newRange, setHigh, subtract)
import Components.Stat exposing (Buff)


{-| Determines what effect a skill has on use
-}
type SkillEffect
    = Damage Float
    | Buff Buff


{-| Create a skill effect that does some damage
-}
damageEffect : Float -> SkillEffect
damageEffect damage =
    Damage damage


{-| Create a skill effect that applies a buff to target
-}
buffEffect : Buff -> SkillEffect
buffEffect buff =
    Buff buff


{-| Skill type. cooldown moves from cooldown time to 0. So a skill with cooldownLeft = 0 is ready to use
-}
type alias Skill t =
    { cooldown : Range Float
    , name : String
    , description : String
    , target : Maybe t
    , effect : SkillEffect
    }


{-| create a new skill with given cooldown, name and description.

Negative cooldowns will be clamped to 0

Empty names and description will use defaults

-}
newSkill : Float -> String -> String -> SkillEffect -> Skill t
newSkill cooldown name description effect =
    let
        defaultString n d =
            if String.isEmpty n then
                d

            else
                n
    in
    Skill
        (newRange 0 cooldown cooldown)
        (defaultString name "Unnamed Skill")
        (defaultString description "Skill description")
        Nothing
        effect


{-| Reset remaining cooldown to max cooldown
-}
resetCooldown : Skill t -> Skill t
resetCooldown skill =
    { skill | cooldown = setHigh skill.cooldown }


{-| Reduce skill cooldown by amount

Negative amounts are ignored

-}
reduceCooldown : Float -> Skill t -> Skill t
reduceCooldown amount skill =
    { skill | cooldown = subtract amount skill.cooldown }


{-| Set skill target.

Will untarget if target is equal to existing target

-}
setTarget : t -> Skill t -> Skill t
setTarget target skill =
    case skill.target of
        Just trgt ->
            if trgt == target then
                unsetTarget skill

            else
                { skill | target = Just target }

        Nothing ->
            { skill | target = Just target }


{-| Set skill target to nothing
-}
unsetTarget : Skill t -> Skill t
unsetTarget skill =
    { skill | target = Nothing }


{-| Is skill ready to use predicate
-}
isReady : Skill t -> Bool
isReady skill =
    let
        hasTarget s =
            case s.target of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    isEmpty skill.cooldown && hasTarget skill
