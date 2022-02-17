module Content.Particles exposing (..)

import Component exposing (Component)
import Components.Physics exposing (initPhysics, withMass, withVelocity)


particle : ( Float, Float ) -> ( Float, Float ) -> List Component
particle ( x, y ) velocity =
    [ Component.physicsComponent
        (initPhysics ( x, y )
            |> withMass 5
            |> withVelocity velocity
        )
    ]
