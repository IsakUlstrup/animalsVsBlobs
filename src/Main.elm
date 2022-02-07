module Main exposing (..)

import Browser
import Browser.Events
import Component exposing (Component, getCharacter)
import Components.Character
import Content.Characters exposing (blob, dog, elephant, mouse, panda)
import Ecs
import GameData exposing (GameScene)
import Html exposing (Html, button, div, h3, input, p)
import Html.Attributes
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
    , speedModifier : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Ecs.emptyScene 10
            |> Ecs.addEntity [ Component.characterComponent (panda ( 0, 0 )) ]
            |> Ecs.addEntity [ Component.characterComponent (dog ( 10, 0 )) ]
            |> Ecs.addEntity [ Component.characterComponent (mouse ( 10, 10 )) ]
            |> Ecs.addEntity [ Component.characterComponent (elephant ( -10, 10 )) ]
            |> Ecs.addEntity [ Component.characterComponent (elephant ( -12, 7 )) ]
            |> Ecs.addEntity [ Component.characterComponent (elephant ( -15, 3 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 40, 20 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 50, 20 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 40, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 30, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 20, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 10, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 11, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 12, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 13, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 14, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 15, 30 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 15, 31 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 15, 32 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 14, 32 )) ]
            |> Ecs.addEntity [ Component.characterComponent (blob ( 12, 32 )) ]
            |> Ecs.addSystem movementSystem
            |> Ecs.addSystem aiSystem
            |> Ecs.addSystem deathSystem
        )
        0
        50
        1
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Tick Float
    | Reset
    | SetSpeedModifier Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | scene =
                    model.scene
                        |> Ecs.runSystems (GameData.GameTick (dt * model.speedModifier))
              }
            , Cmd.none
            )

        Reset ->
            init

        SetSpeedModifier speed ->
            ( { model | speedModifier = speed }, Cmd.none )



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
            case c.appearance of
                Just a ->
                    a

                _ ->
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
        [ Svg.Attributes.style
            ("transform: translate("
                ++ String.fromFloat character.position.x
                ++ "px, "
                ++ String.fromFloat character.position.y
                ++ "px) scale("
                ++ String.fromFloat character.radius
                ++ "); user-select: none;"
            )

        -- , Svg.Attributes.style "user-select: none"
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
            , button [ Html.Events.onClick Reset ] [ text "reset game" ]
            , p [] [ text "game speed: ", text (String.fromFloat model.speedModifier) ]
            , input
                [ Html.Attributes.max "5"
                , Html.Attributes.min "0"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value (String.fromFloat model.speedModifier)
                , Html.Attributes.type_ "range"

                -- , Html.Attributes.step "0.1"
                , Html.Events.onInput (\v -> SetSpeedModifier (String.toFloat v |> Maybe.withDefault model.speedModifier))
                ]
                []
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