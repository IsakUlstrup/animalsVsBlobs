module Content.Buffs exposing (..)

import Components.Stat exposing (Buff, newBuff)


stasis : Maybe Float -> Buff
stasis duration =
    newBuff
        "Stasis"
        "Stops cooldown recovery"
        [ Components.Stat.cooldownRecoveryStat 3 |> Components.Stat.addModifiers [ Components.Stat.setSubModifier 0 ] ]
        duration


speedBoost : Maybe Float -> Buff
speedBoost duration =
    Components.Stat.newBuff
        "Spped boost"
        "Boosts cooldown recovery"
        [ Components.Stat.cooldownRecoveryStat 2 ]
        duration
