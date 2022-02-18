module Systems.PhysicsSystem exposing (physicsSystem)

import Component
import Components.Physics exposing (Physics)
import Components.Vector2 as Vector2 exposing (Vector2)
import Ecs
import GameData exposing (GameMsg, GameScene)


gravityForce : Vector2
gravityForce =
    Vector2.new 0 0.1


windForce : Vector2
windForce =
    Vector2.new 0.05 0


update : Float -> List Physics -> Physics -> Physics
update dt ps physics =
    physics
        |> Components.Physics.applyForces [ Components.Physics.netGravityForce ps physics ]
        -- |> Components.Physics.applyImpulses (Components.Physics.collisionImpulse ps physics)
        |> Components.Physics.move dt
        -- |> Components.Physics.collisionForce ps
        -- |> Components.Physics.collisionMove ps
        |> Components.Physics.constrain -50 50


physicsSystem : GameMsg -> GameScene -> GameScene
physicsSystem msg scene =
    case msg of
        GameData.GameTick dt ->
            let
                physicsObjects =
                    Ecs.mapComponents (\_ c -> Component.getPhysics c) scene |> List.filterMap identity
            in
            scene
                |> Ecs.updateComponents (Component.updatePhysics (update dt physicsObjects))

        _ ->
            scene
