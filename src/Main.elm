module Main exposing (..)

import Browser
import Browser.Dom exposing (Element, Error)
import Browser.Events
import Component exposing (Component, getCharacter)
import Components.Character exposing (CharacterState(..))
import Components.Vector2
import Content.Characters exposing (blob, dog, mouse, panda)
import Ecs
import GameData exposing (GameScene)
import Html exposing (Html, button, div, h3, input, p)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Svg exposing (Attribute, Svg, g, svg, text, text_)
import Svg.Attributes exposing (class, id, viewBox)
import Svg.Events
import Systems.AISystem exposing (aiSystem)
import Systems.MovementSystem exposing (movementSystem)
import Task



---- MODEL ----


type alias Model =
    { scene : GameScene
    , timeAccum : Float
    , tickTime : Float
    , speedModifier : Float
    , renderDebug : Bool
    , deltaCap : Float
    , mousePos : ( Int, Int )
    , viewPort : { x : Float, y : Float }
    }


initScene : GameScene
initScene =
    Ecs.emptyScene 10
        |> Ecs.addEntity (mouse ( 0, 0 ))
        |> Ecs.addEntity (panda ( -10, 20 ))
        |> Ecs.addEntity (dog ( -10, 10 ))
        |> Ecs.addEntity (blob ( -100, 100 ))
        |> Ecs.addEntity (blob ( -110, 100 ))
        |> Ecs.addSystem aiSystem
        |> Ecs.addSystem movementSystem


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
        { x = 0, y = 0 }
    , Browser.Dom.getElement "game-svg" |> Task.attempt GotElement
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
    | SvgClick ( Float, Float )
    | GotElement (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | scene =
                    model.scene
                        |> Ecs.runSystems (GameData.GameTick (min model.deltaCap dt * 0.03 * model.speedModifier))
              }
            , Browser.Dom.getElement "game-svg" |> Task.attempt GotElement
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

        SvgClick ( x, y ) ->
            let
                ( rx, ry ) =
                    ( x - (model.viewPort.x / 2)
                    , y - (model.viewPort.y / 2)
                    )
            in
            ( { model
                | scene =
                    model.scene
                        |> Ecs.runSystems (GameData.MoveTo (Components.Vector2.new rx ry))
              }
            , Cmd.none
            )

        GotElement (Err _) ->
            ( model, Cmd.none )

        GotElement (Ok element) ->
            if element.element.width /= model.viewPort.x || element.element.height /= model.viewPort.y then
                ( { model | viewPort = { x = element.element.width, y = element.element.height } }, Cmd.none )

            else
                ( model, Cmd.none )



---- VIEW ----


viewCharacter : Bool -> Components.Character.Character -> Svg msg
viewCharacter debug character =
    let
        fillColor c =
            if c.player then
                "yellow"

            else
                "rgba(240, 0, 200, 0.6)"

        textLabel c =
            case c.appearance of
                Just a ->
                    a

                _ ->
                    ""

        playerPath c =
            if c.player then
                case c.state of
                    ManualMove trgt ->
                        [ Svg.line
                            [ Svg.Attributes.x2 "0"
                            , Svg.Attributes.y2 "0"
                            , Svg.Attributes.x1 (String.fromFloat (trgt.x - c.position.x))
                            , Svg.Attributes.y1 (String.fromFloat (trgt.y - c.position.y))
                            , Svg.Attributes.stroke "rgba(255, 255, 255, 0.5)"
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeDasharray "0.35, 20"
                            ]
                            []
                        , Svg.circle
                            [ Svg.Attributes.cx (String.fromFloat (trgt.x - c.position.x))
                            , Svg.Attributes.cy (String.fromFloat (trgt.y - c.position.y))
                            , Svg.Attributes.r "0.4"
                            , Svg.Attributes.stroke "white"
                            , Svg.Attributes.strokeWidth "1"
                            , Svg.Attributes.fill "none"
                            ]
                            []
                        ]

                    AiMove trgt ->
                        [ Svg.line
                            [ Svg.Attributes.x2 "0"
                            , Svg.Attributes.y2 "0"
                            , Svg.Attributes.x1 (String.fromFloat (trgt.x - c.position.x))
                            , Svg.Attributes.y1 (String.fromFloat (trgt.y - c.position.y))
                            , Svg.Attributes.stroke "rgba(0, 0, 0, 0.5)"
                            , Svg.Attributes.strokeWidth "5"
                            , Svg.Attributes.strokeLinecap "round"
                            , Svg.Attributes.strokeDasharray "0.35, 20"
                            ]
                            []
                        , Svg.circle
                            [ Svg.Attributes.cx (String.fromFloat (trgt.x - c.position.x))
                            , Svg.Attributes.cy (String.fromFloat (trgt.y - c.position.y))
                            , Svg.Attributes.r "0.4"
                            , Svg.Attributes.stroke "black"
                            , Svg.Attributes.strokeWidth "1"
                            , Svg.Attributes.fill "none"
                            ]
                            []
                        ]

                    _ ->
                        []

            else
                []

        char c =
            if c.player then
                text_
                    ((if c.state /= Idle then
                        [ Svg.Attributes.class "walk-animation", Svg.Attributes.class "player" ]

                      else
                        [ Svg.Attributes.class "player" ]
                     )
                        ++ [ Svg.Attributes.y "15"
                           , Svg.Attributes.fontSize "2.5rem"
                           , Svg.Attributes.textAnchor "middle"
                           ]
                    )
                    [ text (textLabel character) ]

            else
                Svg.circle
                    ((if c.state /= Idle then
                        [ Svg.Attributes.class "blob-walk", Svg.Attributes.class "blob" ]

                      else
                        [ Svg.Attributes.class "blob" ]
                     )
                        ++ [ Svg.Attributes.cx "0"
                           , Svg.Attributes.cy "0"
                           , Svg.Attributes.r "20"
                           , Svg.Attributes.fill (fillColor character)

                           -- , Svg.Attributes.fill "url(#blob-gradient)"
                           -- , Svg.Attributes.class "blob"
                           ]
                    )
                    []

        viewVectors c =
            if debug then
                let
                    vel =
                        Components.Vector2.scale 50 c.velocity

                    acc =
                        Components.Vector2.scale 50 c.acceleration
                in
                [ g []
                    -- <line x1="0" y1="80" x2="100" y2="20" stroke="black" />
                    [ Svg.circle
                        [ Svg.Attributes.cx "0"
                        , Svg.Attributes.cy "0"
                        , Svg.Attributes.r (String.fromFloat c.radius)
                        , Svg.Attributes.stroke "cyan"
                        , Svg.Attributes.strokeWidth "1"
                        , Svg.Attributes.fill "none"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat vel.x)
                        , Svg.Attributes.y2 (String.fromFloat vel.y)
                        , Svg.Attributes.stroke "yellow"
                        , Svg.Attributes.strokeWidth "2"
                        , Svg.Attributes.strokeLinecap "round"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 (String.fromFloat acc.x)
                        , Svg.Attributes.y2 (String.fromFloat acc.y)
                        , Svg.Attributes.stroke "red"
                        , Svg.Attributes.strokeWidth "2"
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
                ++ "px)"
            )
        , Svg.Attributes.class "character"
        ]
        (playerPath character ++ (char character :: viewVectors character))


viewCharacterWrapper : Bool -> ( Ecs.Entity, List ( Ecs.EcsId, Component ) ) -> Maybe (Svg msg)
viewCharacterWrapper debug ( entity, components ) =
    case List.filterMap getCharacter (List.map Tuple.second components) |> List.head of
        Just char ->
            Just (viewCharacter debug char)

        _ ->
            Nothing



-- viewParticleWrapper : Bool -> ( Ecs.Entity, List ( Ecs.EcsId, Component ) ) -> Maybe (Svg msg)
-- viewParticleWrapper debug ( entity, components ) =
--     case List.filterMap getPhysics (List.map Tuple.second components) |> List.head of
--         Just phys ->
--             Just (viewParticle debug phys)
--         _ ->
--             Nothing
-- viewParticle : Bool -> Components.Physics.Physics -> Svg msg
-- viewParticle debug character =
--     let
--         viewVectors c =
--             if debug then
--                 let
--                     vel =
--                         Components.Vector2.scale 3 c.velocity
--                     acc =
--                         Components.Vector2.scale 5 c.acceleration
--                 in
--                 [ g []
--                     -- <line x1="0" y1="80" x2="100" y2="20" stroke="black" />
--                     [ Svg.line
--                         [ Svg.Attributes.x1 "0"
--                         , Svg.Attributes.y1 "0"
--                         , Svg.Attributes.x2 (String.fromFloat vel.x)
--                         , Svg.Attributes.y2 (String.fromFloat vel.y)
--                         , Svg.Attributes.stroke "yellow"
--                         , Svg.Attributes.strokeWidth "0.1"
--                         , Svg.Attributes.strokeLinecap "round"
--                         ]
--                         []
--                     , Svg.line
--                         [ Svg.Attributes.x1 "0"
--                         , Svg.Attributes.y1 "0"
--                         , Svg.Attributes.x2 (String.fromFloat acc.x)
--                         , Svg.Attributes.y2 (String.fromFloat acc.y)
--                         , Svg.Attributes.stroke "red"
--                         , Svg.Attributes.strokeWidth "0.1"
--                         , Svg.Attributes.strokeLinecap "round"
--                         ]
--                         []
--                     ]
--                 ]
--             else
--                 []
--     in
--     g
--         [ Svg.Attributes.style
--             ("transform: translate("
--                 ++ String.fromFloat character.position.x
--                 ++ "px, "
--                 ++ String.fromFloat character.position.y
--                 ++ "px) scale("
--                 ++ String.fromFloat (Components.Physics.getRadius character)
--                 ++ "); user-select: none;"
--             )
--         , Svg.Attributes.class "character"
--         ]
--         (Svg.circle
--             [ Svg.Attributes.cx "0"
--             , Svg.Attributes.cy "0"
--             , Svg.Attributes.r "1"
--             , Svg.Attributes.class "particle"
--             ]
--             []
--             :: viewVectors character
--         )
-- viewGameArea : Svg msg
-- viewGameArea =
--     Svg.rect
--         [ Svg.Attributes.x "-50"
--         , Svg.Attributes.y "-50"
--         , Svg.Attributes.width "100"
--         , Svg.Attributes.height "100"
--         -- , Svg.Attributes.stroke "magenta"
--         -- , Svg.Attributes.strokeWidth "0.5"
--         , Svg.Attributes.fill "#262626"
--         ]
--         []


blobGradient : Svg msg
blobGradient =
    Svg.radialGradient [ Svg.Attributes.id "blob-gradient" ]
        [ Svg.stop [ Svg.Attributes.stopColor "magenta", Svg.Attributes.offset "0" ] []
        , Svg.stop [ Svg.Attributes.stopColor "rgb(177, 0, 177)", Svg.Attributes.offset "10" ] []
        ]


point : Decode.Decoder Msg
point =
    Decode.map2 (\x y -> SvgClick ( x, y ))
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


onClick : Attribute Msg
onClick =
    Svg.Events.on "click" point


viewBoxString : ( Float, Float ) -> String
viewBoxString ( x, y ) =
    [ x - x * 1.5, y - y * 1.5, x, y ] |> List.map String.fromFloat |> List.intersperse " " |> String.concat


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
                [ viewBox (viewBoxString ( model.viewPort.x, model.viewPort.y )), onClick, Svg.Attributes.id "game-svg" ]
                (blobGradient
                    :: (Ecs.mapComponentGroups (viewCharacterWrapper model.renderDebug) model.scene
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
