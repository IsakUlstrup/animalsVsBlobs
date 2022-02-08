module Systems.AISystem exposing (aiSystem)

import Component
import Components.Character
import Ecs
import GameData exposing (GameMsg(..), GameScene)


aiSystem : GameMsg -> GameScene -> GameScene
aiSystem msg scene =
    case msg of
        GameTick _ ->
            let
                characters =
                    Ecs.mapComponents (\_ c -> Component.getCharacter c) scene |> List.filterMap identity
            in
            scene
                |> Ecs.updateComponents (Component.updateCharacter (Components.Character.aiMove characters))

        _ ->
            scene
