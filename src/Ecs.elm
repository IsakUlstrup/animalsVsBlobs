module Ecs exposing
    ( EcsId
    , Entity
    , Scene
    , System
    , addComponents
    , addEntity
    , addSystem
    , emptyScene
    , entityIdToInt
    , filterComponents
    , idToInt
    , mapComponentGroups
    , mapComponents
    , runSystems
    , updateComponent
    , updateComponents
    )

import Random


{-| Id alias for Entity/Component ids
-}
type EcsId
    = EcsId Int


{-| Component, compData is the type stored in each component, usually a union type

This type is not exposed to avoid mixing up Component and compData

-}
type Component compData
    = Component
        { id : EcsId
        , parent : Entity
        , data : compData
        }


{-| Entity is just an id used to associate entities and components
-}
type Entity
    = Entity EcsId


{-| A system is a function that transforms the scene when given a message
-}
type System compData msg
    = System (msg -> Scene compData msg -> Scene compData msg)


{-| A Scene is a container for entities, components and systems
, as well as an id counter to keep track of new ids and a Random seed to help with RNG
-}
type Scene compData msg
    = Scene
        { entities : List Entity
        , components : List (Component compData)
        , systems : List (System compData msg)
        , seed : Random.Seed
        , idCounter : EcsId
        }



---- ECS ID ----


{-| increment id by 1
-}
newId : EcsId -> EcsId
newId (EcsId i) =
    EcsId (i + 1)


{-| id to int, for rendering
-}
idToInt : EcsId -> Int
idToInt (EcsId id) =
    id


{-| Entity to int, for rendering
-}
entityIdToInt : Entity -> Int
entityIdToInt (Entity entity) =
    idToInt entity



---- SCENE ----


{-| Construct new empty scene
-}
emptyScene : Int -> Scene compData msg
emptyScene seedInit =
    Scene
        { entities = []
        , components = []
        , systems = []
        , seed = Random.initialSeed seedInit
        , idCounter = EcsId 0
        }


{-| Increment idCounter by 1
-}
incId : Scene compData msg -> Scene compData msg
incId (Scene scene) =
    Scene { scene | idCounter = newId scene.idCounter }



---- ENTITY ----


addEntity : List compData -> Scene compData msg -> Scene compData msg
addEntity components (Scene scene) =
    let
        entity =
            Entity scene.idCounter
    in
    Scene
        { scene
            | entities = entity :: scene.entities
        }
        |> incId
        |> addComponents entity components



---- COMPONENT ----


{-| Create a new component with parent as parent and compData as data
-}
addComponent : Entity -> compData -> Scene compData msg -> Scene compData msg
addComponent parent compData (Scene scene) =
    Scene
        { scene
            | components = Component { id = scene.idCounter, parent = parent, data = compData } :: scene.components
        }
        |> incId


{-| Add list of component data with parent as parent
-}
addComponents : Entity -> List compData -> Scene compData msg -> Scene compData msg
addComponents parent compDatas scene =
    case compDatas of
        [] ->
            scene

        d :: ds ->
            addComponent parent d (addComponents parent ds scene)


{-| Update a single component with a given function
-}
updateComponent : EcsId -> (compData -> compData) -> Scene compData msg -> Scene compData msg
updateComponent compId f (Scene scene) =
    let
        updateCompData : EcsId -> (compData -> compData) -> Component compData -> Component compData
        updateCompData i func (Component c) =
            if i == c.id then
                Component { c | data = func c.data }

            else
                Component c
    in
    Scene { scene | components = List.map (updateCompData compId f) scene.components }


{-| Update all components with a given function
-}
updateComponents : (compData -> compData) -> Scene compData msg -> Scene compData msg
updateComponents f (Scene scene) =
    let
        updateCompData : (compData -> compData) -> Component compData -> Component compData
        updateCompData func (Component c) =
            Component { c | data = func c.data }
    in
    Scene { scene | components = List.map (updateCompData f) scene.components }


{-| Keep components that satisfy the test.
-}
filterComponents : (compData -> Bool) -> Scene compData msg -> Scene compData msg
filterComponents pred (Scene scene) =
    let
        compDataPred : Component compData -> Bool
        compDataPred (Component c) =
            pred c.data
    in
    Scene { scene | components = List.filter compDataPred scene.components }


{-| Map components
-}
mapComponents : (EcsId -> compData -> a) -> Scene compData msg -> List a
mapComponents f (Scene scene) =
    let
        mapCompData : (EcsId -> compData -> a) -> Component compData -> a
        mapCompData func (Component c) =
            func c.id c.data
    in
    List.map (mapCompData f) scene.components


{-| given a component and a list of ( Entity, List ( EcsId, compData ) ), add component data to one of the lists
-}
compAccum : Component compData -> List ( Entity, List ( EcsId, compData ) ) -> List ( Entity, List ( EcsId, compData ) )
compAccum i list =
    let
        helper : Bool -> Component compData -> List ( Entity, List ( EcsId, compData ) ) -> List ( Entity, List ( EcsId, compData ) ) -> List ( Entity, List ( EcsId, compData ) )
        helper found (Component c) l input =
            case input of
                [] ->
                    if found then
                        l

                    else
                        ( c.parent, [ ( c.id, c.data ) ] ) :: l

                ( entity, components ) :: t ->
                    if c.parent == entity && not found then
                        ( entity, ( c.id, c.data ) :: components ) :: helper True (Component c) l t

                    else
                        ( entity, components ) :: helper found (Component c) l t
    in
    list |> helper False i []


{-| Returns a list of entities and their attached components if they have any
-}
groupComponents : Scene compData msg -> List ( Entity, List ( EcsId, compData ) )
groupComponents (Scene scene) =
    List.foldl compAccum [] scene.components


{-| map components grouped by parent, useful for rendering
-}
mapComponentGroups : (( Entity, List ( EcsId, compData ) ) -> a) -> Scene compData msg -> List a
mapComponentGroups f scene =
    List.map f (groupComponents scene)



---- SYSTEM ----


{-| Add a system to a scene
-}
addSystem : (msg -> Scene c msg -> Scene c msg) -> Scene c msg -> Scene c msg
addSystem system (Scene scene) =
    Scene { scene | systems = System system :: scene.systems }


{-| Run all systems in a scene and return the resulting scene
-}
runSystems : msg -> Scene c msg -> Scene c msg
runSystems msg ((Scene scene) as sc) =
    let
        runSystem : msg -> System c msg -> Scene c msg -> Scene c msg
        runSystem m (System system) s =
            system m s
    in
    List.foldl (runSystem msg) sc scene.systems
