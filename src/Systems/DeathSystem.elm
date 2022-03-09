module Systems.DeathSystem exposing (deathSystem)

import Component
import Components.Character
import Ecs
import GameData exposing (GameMsg(..), GameScene)


deathSystem : GameMsg -> GameScene -> GameScene
deathSystem msg scene =
    case msg of
        GameTick _ ->
            scene

        -- |> Ecs.filterComponents (Component.characterPred Components.Character.isAlive)
        _ ->
            scene
