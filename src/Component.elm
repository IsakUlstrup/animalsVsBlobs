module Component exposing
    ( Component
    , characterComponent
    , characterPred
    , getCharacter
    , getPhysics
    , physicsComponent
    , physicsPred
    , updateAddCharacter
    , updateCharacter
    , updatePhysics
    )

import Components.Character exposing (Character)
import Components.Physics exposing (Physics)


type Component
    = Character Character
    | Physics Physics



---- Character ----


characterComponent : Character -> Component
characterComponent char =
    Character char


getCharacter : Component -> Maybe Character
getCharacter c =
    case c of
        Character ch ->
            Just ch

        _ ->
            Nothing


updateCharacter : (Character -> Character) -> Component -> Component
updateCharacter f c =
    case c of
        Character m ->
            Character (f m)

        _ ->
            c


updateAddCharacter : (Character -> ( Character, List (List Component) )) -> Component -> ( Component, List (List Component) )
updateAddCharacter f c =
    case c of
        Character m ->
            let
                ( char, e ) =
                    f m
            in
            ( Character char, e )

        _ ->
            ( c, [] )


characterPred : (Character -> Bool) -> Component -> Bool
characterPred pred c =
    case c of
        Character char ->
            pred char

        _ ->
            False



---- Physics ----


physicsComponent : Physics -> Component
physicsComponent physics =
    Physics physics


getPhysics : Component -> Maybe Physics
getPhysics c =
    case c of
        Physics p ->
            Just p

        _ ->
            Nothing


updatePhysics : (Physics -> Physics) -> Component -> Component
updatePhysics f c =
    case c of
        Physics p ->
            Physics (f p)

        _ ->
            c


physicsPred : (Physics -> Bool) -> Component -> Bool
physicsPred pred c =
    case c of
        Physics char ->
            pred char

        _ ->
            False
