module Main exposing (..)

import Browser
import Browser.Events
import Component exposing (Component, getCharacter, getPhysics)
import Components.Character
import Components.Physics
import Components.Vector2
import Content.Particles
import Ecs
import GameData exposing (GameScene)
import Html exposing (Html, button, div, h3, input, p)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (Svg, g, svg, text, text_)
import Svg.Attributes exposing (class, id, viewBox)
import Systems.PhysicsSystem exposing (physicsSystem)



---- MODEL ----


type alias Model =
    { scene : GameScene
    , timeAccum : Float
    , tickTime : Float
    , speedModifier : Float
    , renderDebug : Bool
    , deltaCap : Float
    , mousePos : ( Int, Int )
    }


initScene : GameScene
initScene =
    Ecs.emptyScene 10
        |> Ecs.addEntity (Content.Particles.particle ( -30, 0 ) ( 2, 0 ))
        |> Ecs.addEntity (Content.Particles.particle ( -20, 0 ) ( 0.001, 0 ))
        |> Ecs.addEntity (Content.Particles.particle ( 10, 0 ) ( 0.001, 0 ))
        |> Ecs.addEntity (Content.Particles.particle ( 20, 0 ) ( -0.5, -0.5 ))
        |> Ecs.addEntity (Content.Particles.particle ( 30, -35 ) ( 0.3, 0 ))
        |> Ecs.addEntity (Content.Particles.particle ( 10, 3 ) ( -0.5, -1 ))
        |> Ecs.addEntity (Content.Particles.particle ( 13, 9 ) ( -0.5, -0.7 ))
        |> Ecs.addEntity (Content.Particles.advancedParticle ( 17, -9 ) ( -0.2, -0.3 ) 6 3)
        -- |> Ecs.addEntity (Content.Particles.advancedParticle ( 0, 0 ) ( 0, 0 ) 50 10)
        |> Ecs.addEntity (Content.Particles.advancedParticle ( 0, 0 ) ( 0, 0 ) 30 5)
        |> Ecs.addSystem physicsSystem


init : ( Model, Cmd Msg )
init =
    ( Model
        initScene
        0
        50
        1
        True
        100
        ( 0, 0 )
    , Cmd.none
    )



---- UPDATE ----


decodeMouseEvent : Decode.Decoder ( Int, Int )
decodeMouseEvent =
    Decode.map2 (\x y -> ( x, y ))
        (Decode.field "movementX" Decode.int)
        (Decode.field "movementY" Decode.int)


type Msg
    = Tick Float
    | Reset
    | SetSpeedModifier Float
    | SetRenderDebug Bool
    | MouseMove ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | scene =
                    model.scene
                        |> Ecs.runSystems (GameData.GameTick (min model.deltaCap dt * 0.03 * model.speedModifier))
              }
            , Cmd.none
            )

        Reset ->
            ( { model | scene = initScene }, Cmd.none )

        SetSpeedModifier speed ->
            ( { model | speedModifier = speed }, Cmd.none )

        SetRenderDebug flag ->
            ( { model | renderDebug = flag }, Cmd.none )

        MouseMove ( x, y ) ->
            ( { model
                | mousePos = ( Tuple.first model.mousePos + x, Tuple.second model.mousePos + y )
              }
            , Cmd.none
            )



---- VIEW ----


viewCharacter : Bool -> Components.Character.Character -> Svg msg
viewCharacter debug character =
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

        viewVectors c =
            if debug then
                let
                    vel =
                        Components.Vector2.scale 2 c.velocity

                    acc =
                        Components.Vector2.scale 2 c.acceleration
                in
                [ g []
                    -- <line x1="0" y1="80" x2="100" y2="20" stroke="black" />
                    [ Svg.circle
                        [ Svg.Attributes.cx "0"
                        , Svg.Attributes.cy "0"
                        , Svg.Attributes.r "1"
                        , Svg.Attributes.stroke "cyan"
                        , Svg.Attributes.strokeWidth "0.1"
                        , Svg.Attributes.fill "none"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat vel.x)
                        , Svg.Attributes.y2 (String.fromFloat vel.y)
                        , Svg.Attributes.stroke "yellow"
                        , Svg.Attributes.strokeWidth "0.1"
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat acc.x)
                        , Svg.Attributes.y2 (String.fromFloat acc.y)
                        , Svg.Attributes.stroke "red"
                        , Svg.Attributes.strokeWidth "0.1"
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                    ]
                ]

            else
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
        , Svg.Attributes.class "character"
        ]
        (char character :: viewVectors character)


viewCharacterWrapper : Bool -> ( Ecs.Entity, List ( Ecs.EcsId, Component ) ) -> Maybe (Svg msg)
viewCharacterWrapper debug ( entity, components ) =
    case List.filterMap getCharacter (List.map Tuple.second components) |> List.head of
        Just char ->
            Just (viewCharacter debug char)

        _ ->
            Nothing


viewParticleWrapper : Bool -> ( Ecs.Entity, List ( Ecs.EcsId, Component ) ) -> Maybe (Svg msg)
viewParticleWrapper debug ( entity, components ) =
    case List.filterMap getPhysics (List.map Tuple.second components) |> List.head of
        Just phys ->
            Just (viewParticle debug phys)

        _ ->
            Nothing


viewParticle : Bool -> Components.Physics.Physics -> Svg msg
viewParticle debug character =
    let
        viewVectors c =
            if debug then
                let
                    vel =
                        Components.Vector2.scale 3 c.velocity

                    acc =
                        Components.Vector2.scale 5 c.acceleration
                in
                [ g []
                    -- <line x1="0" y1="80" x2="100" y2="20" stroke="black" />
                    [ Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat vel.x)
                        , Svg.Attributes.y2 (String.fromFloat vel.y)
                        , Svg.Attributes.stroke "yellow"
                        , Svg.Attributes.strokeWidth "0.1"
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat acc.x)
                        , Svg.Attributes.y2 (String.fromFloat acc.y)
                        , Svg.Attributes.stroke "red"
                        , Svg.Attributes.strokeWidth "0.1"
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                    ]
                ]

            else
                []
    in
    g
        [ Svg.Attributes.style
            ("transform: translate("
                ++ String.fromFloat character.position.x
                ++ "px, "
                ++ String.fromFloat character.position.y
                ++ "px) scale("
                ++ String.fromFloat (Components.Physics.getRadius character)
                ++ "); user-select: none;"
            )
        , Svg.Attributes.class "character"
        ]
        (Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "1"
            , Svg.Attributes.class "particle"
            ]
            []
            :: viewVectors character
        )


viewGameArea : Svg msg
viewGameArea =
    Svg.rect
        [ Svg.Attributes.x "-50"
        , Svg.Attributes.y "-50"
        , Svg.Attributes.width "100"
        , Svg.Attributes.height "100"

        -- , Svg.Attributes.stroke "magenta"
        -- , Svg.Attributes.strokeWidth "0.5"
        , Svg.Attributes.fill "#262626"
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
                , Html.Events.onInput (\v -> SetSpeedModifier (String.toFloat v |> Maybe.withDefault model.speedModifier))
                ]
                []
            , p []
                [ text "render debug: "
                , input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.value
                        (if model.renderDebug then
                            "True"

                         else
                            "False"
                        )
                    , Html.Events.onClick (SetRenderDebug (not model.renderDebug))
                    ]
                    []
                ]
            ]
        , div [ id "game-container" ]
            [ svg
                [ viewBox "-50 -50 100 100" ]
                (viewGameArea
                    :: blobGradient
                    :: (Ecs.mapComponentGroups (viewParticleWrapper model.renderDebug) model.scene
                            |> List.filterMap identity
                       )
                )
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
