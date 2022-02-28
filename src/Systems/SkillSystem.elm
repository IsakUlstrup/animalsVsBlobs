module Systems.SkillSystem exposing (..)

import Component exposing (Component)
import Components.Character exposing (Character)
import Content.Characters exposing (panda)
import Ecs
import GameData exposing (GameMsg(..), GameScene)


updateSkill : Float -> Character -> ( Character, List (List Component) )
updateSkill _ char =
    ( char, [ panda ( 0, 0 ) ] )


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            -- let
            --     characters =
            --         Ecs.mapComponents (\_ c -> Component.getCharacter c) scene |> List.filterMap identity
            -- in
            scene
                -- |> Ecs.updateComponents (Component.updateCharacter (Components.Character.collision characters))
                |> Ecs.updateAddComponents (Component.updateAddCharacter (updateSkill dt))

        _ ->
            scene
