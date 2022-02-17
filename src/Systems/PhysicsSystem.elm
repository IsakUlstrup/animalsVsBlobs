module Systems.PhysicsSystem exposing (physicsSystem)

import Component
import Components.Physics exposing (Physics)
import Components.Vector2 as Vector2
import Ecs
import GameData exposing (GameMsg, GameScene)


update : Float -> Physics -> Physics
update dt physics =
    physics
        |> Components.Physics.applyForces [ Vector2.new 0 0.1, Vector2.new 0.3 0 ]
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
