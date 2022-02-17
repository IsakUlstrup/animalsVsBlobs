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


update : Float -> Physics -> Physics
update dt physics =
    physics
        |> Components.Physics.applyForces [ gravityForce, windForce ]
        |> Components.Physics.move dt
        |> Components.Physics.constrain -50 50


physicsSystem : GameMsg -> GameScene -> GameScene
physicsSystem msg scene =
    case msg of
        GameData.GameTick dt ->
            scene
                |> Ecs.updateComponents (Component.updatePhysics (update dt))

        _ ->
            scene
