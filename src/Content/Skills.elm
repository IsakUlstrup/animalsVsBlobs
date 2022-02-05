module Content.Skills exposing (..)

import Components.Skill exposing (Skill, newSkill)
import Content.Buffs


emp : Skill t
emp =
    newSkill 1000
        "EMP"
        "disables skill cooldown recovery"
        (Components.Skill.buffEffect (Content.Buffs.stasis (Just 5000)))


laser : Skill t
laser =
    Components.Skill.newSkill 1000
        "Laser"
        "dps"
        (Components.Skill.damageEffect 10)


speedBoost : Skill t
speedBoost =
    Components.Skill.newSkill 1000
        "Speed boost"
        "boosts skill cooldown recovery"
        (Components.Skill.buffEffect (Content.Buffs.speedBoost (Just 1000)))
