module Systems.SkillSystem exposing (..)

import Component exposing (Component)
import Components.Character exposing (Character)
import Content.Characters exposing (rocket)
import Ecs
import GameData exposing (GameMsg(..), GameScene)


sysUpdateSkill : Float -> List Character -> Character -> ( Character, List (List Component) )
sysUpdateSkill dt chars char =
    let
        newChar =
            Components.Character.updateCharacterSkill dt char
    in
    if Components.Character.skillIsReady newChar && (List.filter (Components.Character.isWithinVision char) chars |> List.isEmpty |> not) then
        ( Components.Character.useSkill newChar, [ rocket ( char.position.x, char.position.y ) ] )

    else
        ( newChar, [] )


skillSystem : GameMsg -> GameScene -> GameScene
skillSystem msg scene =
    case msg of
        GameTick dt ->
            let
                characters =
                    Ecs.mapComponents (\_ c -> Component.getCharacter c) scene |> List.filterMap identity
            in
            scene
                |> Ecs.updateAddComponents (Component.updateAddCharacter (sysUpdateSkill dt characters))

        _ ->
            scene
