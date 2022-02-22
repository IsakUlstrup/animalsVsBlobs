module Systems.MovementSystem exposing (..)

import Component
import Components.Character exposing (Character, CharacterState(..))
import Components.Vector2 exposing (Vector2)
import Ecs
import GameData exposing (GameMsg(..), GameScene)


setMoveTarget : Vector2 -> Character -> Character
setMoveTarget target character =
    if character.player then
        Components.Character.setMoveTargetVector target character

    else
        character


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
                |> Ecs.updateComponents (Component.updateCharacter (setMoveTarget target))

        _ ->
            scene
