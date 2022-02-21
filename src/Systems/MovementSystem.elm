module Systems.MovementSystem exposing (..)

import Component
import Components.Character
import Ecs
import GameData exposing (GameMsg(..), GameScene)


movementSystem : GameMsg -> GameScene -> GameScene
movementSystem msg scene =
    case msg of
        GameData.GameTick dt ->
            let
                characters =
                    Ecs.mapComponents (\_ c -> Component.getCharacter c) scene |> List.filterMap identity
            in
            scene
                |> Ecs.updateComponents (Component.updateCharacter (Components.Character.collision characters))
                |> Ecs.updateComponents (Component.updateCharacter (Components.Character.update dt))

        MoveTo target ->
            scene
                |> Ecs.updateComponents (Component.updateCharacter (Components.Character.setMoveTargetVector target))

        _ ->
            scene
