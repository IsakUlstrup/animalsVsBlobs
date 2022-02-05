module Component exposing (Component, characterComponent, characterPred, getCharacter, updateCharacter)

import Components.Character exposing (Character)


type Component
    = Character Character



---- Character ----


characterComponent : Character -> Component
characterComponent char =
    Character char


getCharacter : Component -> Maybe Character
getCharacter c =
    case c of
        Character ch ->
            Just ch


updateCharacter : (Character -> Character) -> Component -> Component
updateCharacter f c =
    case c of
        Character m ->
            Character (f m)


characterPred : (Character -> Bool) -> Component -> Bool
characterPred pred c =
    case c of
        Character char ->
            pred char
