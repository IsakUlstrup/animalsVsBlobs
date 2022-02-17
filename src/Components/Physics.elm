module Components.Physics exposing
    ( Physics
    , applyForces
    , constrain
    , getRadius
    , initPhysics
    , move
    , withAcceleration
    , withMass
    , withPlayer
    , withVelocity
    , withVelocityVector
    )

import Components.Vector2 as Vector2 exposing (Vector2)


type alias Physics =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , mass : Float
    , player : Bool
    }



---- BUILDER ----


initPhysics : ( Float, Float ) -> Physics
initPhysics ( x, y ) =
    Physics (Vector2.new x y)
        (Vector2.new 0 0)
        (Vector2.new 0 0)
        1
        False


withVelocity : ( Float, Float ) -> Physics -> Physics
withVelocity ( x, y ) physics =
    { physics | velocity = Vector2.new x y }


withVelocityVector : Vector2 -> Physics -> Physics
withVelocityVector velocity physics =
    { physics | velocity = velocity }


withAcceleration : ( Float, Float ) -> Physics -> Physics
withAcceleration ( x, y ) physics =
    { physics | acceleration = Vector2.new x y }


withMass : Float -> Physics -> Physics
withMass mass physics =
    { physics | mass = mass }


withPlayer : Bool -> Physics -> Physics
withPlayer player physics =
    { physics | player = player }



---- UPDATES ----


move : Float -> Physics -> Physics
move dt phys =
    { phys
        | velocity = Vector2.add phys.velocity (Vector2.scale dt phys.acceleration)
        , position = Vector2.add phys.position (Vector2.scale dt phys.velocity)
    }


applyForces : List Vector2 -> Physics -> Physics
applyForces forces physics =
    { physics | acceleration = List.foldl Vector2.add (Vector2.new 0 0) forces }


constrainY : Float -> Float -> Float -> Physics -> Physics
constrainY low high radius physics =
    if physics.position.y >= (high - radius) then
        { physics
            | position = Vector2.setY (high - radius) physics.position

            -- , acceleration = physics.acceleration |> Vector2.negateY
            , velocity = Vector2.negateY physics.velocity
        }

    else if physics.position.y <= low + radius then
        { physics
            | position = Vector2.setY (low + radius) physics.position

            -- , acceleration = physics.acceleration |> Vector2.negateY
            , velocity = Vector2.negateY physics.velocity
        }

    else
        physics


constrainX : Float -> Float -> Float -> Physics -> Physics
constrainX low high radius physics =
    if physics.position.x >= (high - radius) then
        { physics
            | position = Vector2.setX (high - radius) physics.position
            , velocity = Vector2.negateX physics.velocity
        }

    else if physics.position.x <= low + radius then
        { physics
            | position = Vector2.setX (low + radius) physics.position
            , velocity = Vector2.negateX physics.velocity
        }

    else
        physics


constrain : Float -> Float -> Physics -> Physics
constrain low high physics =
    physics
        |> constrainY low high (getRadius physics)
        |> constrainX low high (getRadius physics)



---- GETTERS ----


getRadius : Physics -> Float
getRadius physics =
    sqrt physics.mass
