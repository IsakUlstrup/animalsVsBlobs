module Content.Particles exposing (..)

import Component exposing (Component)
import Components.Physics exposing (initPhysics, withDensity, withMass, withVelocity)


particle : ( Float, Float ) -> ( Float, Float ) -> List Component
particle ( x, y ) velocity =
    [ Component.physicsComponent
        (initPhysics ( x, y )
            |> withMass 1
            |> withVelocity velocity
        )
    ]


advancedParticle : ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> List Component
advancedParticle ( x, y ) velocity mass density =
    [ Component.physicsComponent
        (initPhysics ( x, y )
            |> withMass mass
            |> withVelocity velocity
            |> withDensity density
        )
    ]
