module Main exposing (..)

import Browser
import Browser.Events
import Component exposing (Component, getCharacter)
import Components.Character
import Ecs
import GameData exposing (GameScene)
import Html exposing (Html, button, div, h3, p)
import Html.Events
import Svg exposing (Svg, g, svg, text, text_)
import Svg.Attributes exposing (class, id, viewBox)
import Systems.AISystem exposing (aiSystem)
import Systems.DeathSystem exposing (deathSystem)
import Systems.MovementSystem exposing (movementSystem)



---- MODEL ----


type alias Model =
    { scene : GameScene
    , timeAccum : Float
    , tickTime : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Ecs.emptyScene 10
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newPlayerCharacter ( 0, 0 ) ( 0, 0 ) 4 0.009) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newPlayerCharacter ( 10, 5 ) ( 0, 0 ) 4 0.007) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newPlayerCharacter ( -5, 2 ) ( 0, 0 ) 4 0.01) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newPlayerCharacter ( 2, 40 ) ( 0, 0 ) 4 0.006) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( 40, -20 ) ( 0, 0 ) 2) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( 40, 40 ) ( 0, 0 ) 3) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( -40, -40 ) ( 0, 0 ) 2) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( -30, -44 ) ( 0, 0 ) 1) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( -25, 30 ) ( 0, 0 ) 2) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( -15, 30 ) ( 0, 0 ) 5) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( -45, 30 ) ( 0, 0 ) 2) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( -45, 40 ) ( 0, 0 ) 3) ]
            |> Ecs.addEntity [ Component.characterComponent (Components.Character.newCharacter ( 45, -40 ) ( 0, 0 ) 1) ]
            |> Ecs.addSystem movementSystem
            |> Ecs.addSystem aiSystem
            |> Ecs.addSystem deathSystem
        )
        0
        16
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Float
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                newTimeAccum =
                    dt + model.timeAccum
            in
            if newTimeAccum >= model.tickTime then
                ( { model
                    | scene = model.scene |> Ecs.runSystems (GameData.GameTick model.tickTime)
                    , timeAccum = 0
                  }
                , Cmd.none
                )

            else
                ( { model | timeAccum = newTimeAccum }, Cmd.none )

        Reset ->
            init



---- VIEW ----


viewCharacter : Components.Character.Character -> Svg msg
viewCharacter character =
    let
        fillColor c =
            if c.player then
                "yellow"

            else
                "magenta"

        textLabel c =
            if c.player then
                "🐼"

            else
                ""

        char c =
            if c.player then
                text_
                    [ Svg.Attributes.y "0.75"
                    , Svg.Attributes.fontSize "0.14rem"
                    , Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.class "player"
                    ]
                    [ text (textLabel character) ]

            else
                Svg.circle
                    [ Svg.Attributes.cx "0"
                    , Svg.Attributes.cy "0"
                    , Svg.Attributes.r "1"
                    , Svg.Attributes.fill (fillColor character)

                    -- , Svg.Attributes.fill "url(#blob-gradient)"
                    , Svg.Attributes.class "blob"
                    ]
                    []
    in
    g
        [ Svg.Attributes.transform
            ("translate("
                ++ String.fromFloat character.position.x
                ++ " "
                ++ String.fromFloat character.position.y
                ++ ") scale("
                ++ String.fromFloat character.radius
                ++ ")"
            )
        , Svg.Attributes.style "user-select: none"
        , Svg.Attributes.class "character"
        ]
        [ char character ]


viewCharacterWrapper : ( Ecs.Entity, List ( Ecs.EcsId, Component ) ) -> Maybe (Svg msg)
viewCharacterWrapper ( entity, components ) =
    case List.filterMap getCharacter (List.map Tuple.second components) |> List.head of
        Just char ->
            Just (viewCharacter char)

        _ ->
            Nothing


viewGameArea : Svg msg
viewGameArea =
    Svg.rect
        [ Svg.Attributes.x "-50"
        , Svg.Attributes.y "-50"
        , Svg.Attributes.width "100"
        , Svg.Attributes.height "100"
        , Svg.Attributes.fill "#262626"
        , Svg.Attributes.stroke "cyan"
        , Svg.Attributes.strokeWidth "1"
        ]
        []


blobGradient : Svg msg
blobGradient =
    Svg.radialGradient [ Svg.Attributes.id "blob-gradient" ]
        [ Svg.stop [ Svg.Attributes.stopColor "magenta", Svg.Attributes.offset "0" ] []
        , Svg.stop [ Svg.Attributes.stopColor "rgb(177, 0, 177)", Svg.Attributes.offset "10" ] []
        ]


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "game-ui" ]
            [ h3 [] [ text "Game UI" ]
            , p [] [ text "Lorem ipsum etc" ]
            , button [ Html.Events.onClick Reset ] [ text "reset game" ]
            ]
        , div [ id "game-container" ]
            [ svg
                [ viewBox "-50 -50 100 100"
                , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                ]
                (viewGameArea
                    :: blobGradient
                    :: (Ecs.mapComponentGroups viewCharacterWrapper model.scene
                            |> List.filterMap identity
                       )
                )
            ]
        , div [ class "game-ui" ]
            [ h3 [] [ text "Game UI" ]
            , p [] [ text "Lorem ipsum etc" ]
            ]
        ]



---- SUBS ----


subs : Model -> Sub Msg
subs _ =
    Browser.Events.onAnimationFrameDelta Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subs
        }
