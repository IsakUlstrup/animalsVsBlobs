module Components.Part exposing (..)

import Components.Color exposing (Color, initColor)
import Components.Range exposing (Range, isEmpty, newRange, subtract)
import Components.Skill exposing (Skill, SkillEffect(..))
import Components.Stat exposing (Buff, Stat, getStatValue, getSumStats)


type alias Part t =
    { name : String
    , skills : List (Skill t)
    , stats : List Buff
    , durability : Range Float
    , color : Color
    }


newPart : String -> List (Skill t) -> List Buff -> Part t
newPart name skills buffs =
    Part name skills buffs (newRange 0 100 100) initColor


{-| Reduce all part skill cooldowns based on cooldown recovery stat
-}
reduceSkillCooldown : Float -> Part t -> Part t
reduceSkillCooldown amount part =
    let
        cooldownRate : Part t -> Float
        cooldownRate p =
            List.concatMap (\s -> s.effects) p.stats
                |> Components.Stat.getSumCooldownRecovery
                |> Maybe.andThen (\s -> Just (getStatValue s))
                |> Maybe.withDefault 1
    in
    { part | skills = List.map (Components.Skill.reduceCooldown (amount * cooldownRate part)) part.skills }


resetSkillCooldown : Int -> Part t -> Part t
resetSkillCooldown skillIndex part =
    { part
        | skills =
            List.indexedMap
                (\i s ->
                    if i == skillIndex then
                        Components.Skill.resetCooldown s

                    else
                        s
                )
                part.skills
    }


{-| Reduce buff remaining time, remove any that are expired
-}
reduceBuffDuration : Float -> Part t -> Part t
reduceBuffDuration amount part =
    { part
        | stats =
            part.stats
                |> List.map (Components.Stat.reduceDuration amount)
                |> List.filter Components.Stat.isActive
    }


{-| Set target of all part skills
-}
setSkillTarget : t -> Part t -> Part t
setSkillTarget target part =
    { part | skills = List.map (Components.Skill.setTarget target) part.skills }


{-| Add buff to part
-}
addBuff : Buff -> Part t -> Part t
addBuff buff part =
    { part | stats = buff :: part.stats }


{-| Apply damage to part durability
-}
damage : Float -> Part t -> Part t
damage amount part =
    { part | durability = subtract amount part.durability }


{-| Get a list of stats on part
-}
getStats : Part t -> List Stat
getStats part =
    List.concatMap (\b -> b.effects) part.stats |> getSumStats


{-| Is part durability not empty
-}
isAlive : Part t -> Bool
isAlive part =
    isEmpty part.durability |> not
