module Components.Stat exposing
    ( Buff
    , Stat
    , StatModifier(..)
    , StatType(..)
    , addModifier
    , addModifiers
    , cooldownRecoveryStat
    , getStatValue
    , getSumCooldownRecovery
    , getSumPower
    , getSumStats
    , isActive
    , newBuff
    , powerStat
    , reduceDuration
    , setSubModifier
    , subModifier
    )

import Components.Range exposing (Range, newRange)


type StatModifier
    = Add Float
    | Subtract Float
    | Multiply Float
    | SetMulti Float


{-| Create add multiplier, negative values will be clamped to 0
-}
addModifier : Float -> StatModifier
addModifier i =
    Add (max 0 i)


{-| Create subtract multiplier, negative values will be clamped to 0
-}
subModifier : Float -> StatModifier
subModifier i =
    Subtract (max 0 i)


setSubModifier : Float -> StatModifier
setSubModifier i =
    SetMulti (max 0 i)


type StatType
    = Power
    | CooldownRecovery


type alias Stat =
    { statType : StatType
    , modifiers : List StatModifier
    }


{-| Create a new power stat
-}
powerStat : Float -> Stat
powerStat init =
    Stat Power [ Add init ]


{-| Create a new cooldown recovery stat
-}
cooldownRecoveryStat : Float -> Stat
cooldownRecoveryStat init =
    Stat CooldownRecovery [ Add init ]


add : List StatModifier -> Float -> Float
add mods sum =
    let
        addAccum : StatModifier -> Float -> Float
        addAccum mod s =
            case mod of
                Add i ->
                    s + abs i

                Subtract i ->
                    s - abs i

                _ ->
                    s
    in
    List.foldl addAccum 0 mods + sum


multi : List StatModifier -> Float -> Float
multi mods sum =
    let
        multiAccum : StatModifier -> Float -> Float
        multiAccum mod s =
            case mod of
                Multiply i ->
                    s + i

                _ ->
                    s
    in
    List.foldl multiAccum 1 mods * sum


setMulti : List StatModifier -> Float -> Float
setMulti mods sum =
    let
        multiAccum : StatModifier -> Float -> Float
        multiAccum mod s =
            case mod of
                SetMulti i ->
                    s * i

                _ ->
                    s
    in
    List.foldl multiAccum 1 mods * sum


{-| Compute stat value based on modifiers. order of operations is add/subtract -> multiply -> set multiply

If a SetMultiplier of 0 is included, result will always be 0

-}
getStatValue : Stat -> Float
getStatValue stat =
    0
        |> add stat.modifiers
        |> multi stat.modifiers
        |> setMulti stat.modifiers


{-| Stat equality, useful for filtering similar stats
-}
statEq : Stat -> Stat -> Bool
statEq stat1 stat2 =
    case stat1.statType of
        Power ->
            case stat2.statType of
                Power ->
                    True

                _ ->
                    False

        CooldownRecovery ->
            case stat2.statType of
                CooldownRecovery ->
                    True

                _ ->
                    False


{-| Add modifiers to stat
-}
addModifiers : List StatModifier -> Stat -> Stat
addModifiers mods stat =
    { stat | modifiers = mods ++ stat.modifiers }


{-| combine the modifiers of two stats if they are of same type
-}
statCombine : Stat -> Stat -> Stat
statCombine stat1 stat2 =
    case stat1.statType of
        Power ->
            case stat2.statType of
                Power ->
                    { stat1
                        | modifiers = stat1.modifiers ++ stat2.modifiers
                    }

                _ ->
                    stat1

        CooldownRecovery ->
            case stat2.statType of
                CooldownRecovery ->
                    { stat1
                        | modifiers = stat1.modifiers ++ stat2.modifiers
                    }

                _ ->
                    stat1


{-| Given a target stat and a list of stats, return the sum of stats that match target
-}
getSumStat : Stat -> List Stat -> Maybe Stat
getSumStat targetStat stats =
    case List.filter (statEq targetStat) stats of
        [] ->
            Nothing

        matchingStats ->
            Just (List.foldl statCombine targetStat matchingStats)


{-| Given a list of stats, return the sum of all power stats if any
-}
getSumPower : List Stat -> Maybe Stat
getSumPower stats =
    getSumStat (powerStat 0) stats


{-| Given a list of stats, return the sum of all cooldown recovery stats if any
-}
getSumCooldownRecovery : List Stat -> Maybe Stat
getSumCooldownRecovery stats =
    getSumStat (cooldownRecoveryStat 0) stats


getSumStats : List Stat -> List Stat
getSumStats stats =
    List.filterMap identity [ getSumPower stats, getSumCooldownRecovery stats ]



---- BUFF ----


type alias Buff =
    { name : String
    , description : String
    , effects : List Stat
    , duration : Maybe (Range Float)
    }


{-| Buff constructor
-}
newBuff : String -> String -> List Stat -> Maybe Float -> Buff
newBuff name description effects duration =
    let
        stringWithDefault d s =
            if String.isEmpty s then
                d

            else
                s
    in
    case duration of
        Just dur ->
            Buff
                (name |> stringWithDefault "Unnamed buff")
                (description |> stringWithDefault "Buff description")
                effects
                (Just
                    (newRange
                        0
                        dur
                        dur
                    )
                )

        Nothing ->
            Buff
                (name |> stringWithDefault "Unnamed buff")
                (description |> stringWithDefault "Buff description")
                effects
                Nothing


{-| Reduce remaining duration if not infinite
-}
reduceDuration : Float -> Buff -> Buff
reduceDuration amount buff =
    case buff.duration of
        Just duration ->
            { buff | duration = Just (Components.Range.subtract amount duration) }

        Nothing ->
            buff


{-| Is buff active?
-}
isActive : Buff -> Bool
isActive buff =
    case buff.duration of
        Just duration ->
            Components.Range.isEmpty duration |> not

        Nothing ->
            True
