module Components.Core exposing (Core, Faction(..), damage, isAlive, isPlayer, newCore, newFactionCore, newPlayerCore)

import Components.Range as Range exposing (Range, newRange)


type Faction
    = Faction1
    | Faction2
    | Neutral
    | Player Bool


type alias Core =
    { durability : Range Float
    , faction : Faction
    }


{-| Core constructor
-}
newCore : Faction -> Core
newCore faction =
    Core (newRange 0 100 100) faction


{-| player core constructor, controllable flag determined if core represets a controllable entity
-}
newPlayerCore : Bool -> Core
newPlayerCore controllable =
    newCore (Player controllable)


{-| faction core constructor, hard coded to Faction1 for now
-}
newFactionCore : Core
newFactionCore =
    newCore Faction1


{-| damage core by amount
-}
damage : Float -> Core -> Core
damage amount core =
    { core | durability = Range.subtract amount core.durability }


{-| Does core have durability left?
-}
isAlive : Core -> Bool
isAlive core =
    Range.isEmpty core.durability |> not


{-| Is core faction player
TODO: this should handle list of cores
-}
isPlayer : Core -> Bool
isPlayer core =
    case core.faction of
        Player _ ->
            True

        _ ->
            False
