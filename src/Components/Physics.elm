module Components.Physics exposing
    ( Physics
    , applyForces
    , applyImpulses
    , collisionImpulse
    , constrain
    , getRadius
    , gravity
    , gravityStrength
    , initPhysics
    , move
    , netGravityForce
    , withAcceleration
    , withDensity
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
    , density : Float
    , player : Bool
    }



---- BUILDER ----


initPhysics : ( Float, Float ) -> Physics
initPhysics ( x, y ) =
    Physics (Vector2.new x y)
        (Vector2.new 0 0)
        (Vector2.new 0 0)
        1
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


withDensity : Float -> Physics -> Physics
withDensity density physics =
    { physics | density = density }


withPlayer : Bool -> Physics -> Physics
withPlayer player physics =
    { physics | player = player }



---- UPDATES ----


move : Float -> Physics -> Physics
move dt phys =
    { phys
        | velocity = Vector2.add phys.velocity phys.acceleration
        , position = Vector2.add phys.position (Vector2.scale dt phys.velocity)
    }


applyForces : List Vector2 -> Physics -> Physics
applyForces forces physics =
    let
        force =
            List.foldl Vector2.add (Vector2.new 0 0) forces
                |> Vector2.divide physics.mass
    in
    { physics | acceleration = Vector2.divide (Vector2.magnitude physics.velocity / physics.mass) force }


applyImpulses : List Vector2 -> Physics -> Physics
applyImpulses impulses phys =
    { phys | velocity = List.foldl Vector2.add phys.velocity impulses }


constrainY : Float -> Float -> Float -> Physics -> Physics
constrainY low high radius physics =
    if physics.position.y > (high - radius) then
        { physics
            | position = Vector2.setY (high - radius) physics.position

            -- , acceleration = physics.acceleration |> Vector2.negateY
            , velocity = Vector2.negateScaleY 0.95 physics.velocity
        }

    else if physics.position.y < low + radius then
        { physics
            | position = Vector2.setY (low + radius) physics.position

            -- , acceleration = physics.acceleration |> Vector2.negateY
            , velocity = Vector2.negateScaleY 0.95 physics.velocity
        }

    else
        physics


constrainX : Float -> Float -> Float -> Physics -> Physics
constrainX low high radius physics =
    if physics.position.x > (high - radius) then
        { physics
            | position = Vector2.setX (high - radius) physics.position
            , velocity = Vector2.negateScaleX 0.95 physics.velocity
        }

    else if physics.position.x < low + radius then
        { physics
            | position = Vector2.setX (low + radius) physics.position
            , velocity = Vector2.negateScaleX 0.95 physics.velocity
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
    physics.mass / physics.density



---- GRAVITY ----


gravityStrength : Physics -> Physics -> Float
gravityStrength target body =
    let
        dist =
            Vector2.distance target.position body.position

        grav =
            5
    in
    if dist <= 0 then
        0

    else
        (target.mass * body.mass) / (dist ^ 2) * grav


gravity : Physics -> Physics -> Vector2
gravity target body =
    Vector2.direction body.position target.position
        |> Vector2.setMagnitude (gravityStrength target body)


netGravityForce : List Physics -> Physics -> Vector2
netGravityForce ps phys =
    List.map
        (\p -> gravity p phys)
        ps
        |> List.foldl Vector2.add (Vector2.new 0 0)



---- COLLISION ----


isColliding : Physics -> Physics -> Bool
isColliding c1 c2 =
    Vector2.distance c1.position c2.position < getRadius c1 + getRadius c2


collisionImpulse : List Physics -> Physics -> List Vector2
collisionImpulse chars char =
    List.foldl
        (\c1 sum ->
            if c1 /= char then
                if isColliding c1 char then
                    let
                        e =
                            0.8

                        -- d =
                        --     Vector2.distance char.position c1.position - (getRadius c1 + getRadius char) |> abs
                        vrel =
                            Vector2.subtract char.velocity c1.velocity

                        normal =
                            Vector2.direction char.position c1.position

                        impulseMagnitude =
                            -(1 + e) * Vector2.dot vrel normal / ((1 / char.mass) + (1 / c1.mass))

                        jn =
                            Vector2.setMagnitude impulseMagnitude normal |> Vector2.divide char.mass
                    in
                    jn :: sum

                else
                    sum

            else
                sum
        )
        []
        chars
