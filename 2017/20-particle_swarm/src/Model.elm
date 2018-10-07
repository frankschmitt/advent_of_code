module Model exposing (..)

import List.Extra exposing (minimumBy)


type Msg
    = Step
    | Run
    | ToggleRemoveColliders


type alias Vector3D =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Particle =
    { index : Int
    , position : Vector3D
    , velocity : Vector3D
    , acceleration : Vector3D
    }


type alias ParticleSystem =
    { particles : List Particle
    , closestToOrigin : Maybe Particle
    , iteration : Int
    , iterationsSinceLastCTOChange : Int
    }



-- move the given 3D vector by the given amount (using vector addition)


move : Vector3D -> Vector3D -> Vector3D
move v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y, z = v1.z + v2.z }



-- perform the next step for a given particle


step : Particle -> Particle
step { index, position, velocity, acceleration } =
    let
        v_x =
            velocity.x + acceleration.x

        v_y =
            velocity.y + acceleration.y

        v_z =
            velocity.z + acceleration.z

        x =
            position.x + v_x

        y =
            position.y + v_y

        z =
            position.z + v_z
    in
    { index = index
    , position = { x = x, y = y, z = z }
    , velocity = { x = v_x, y = v_y, z = v_z }
    , acceleration = acceleration
    }


{-| Compute the next step for a given particle list
-}
stepList : List Particle -> List Particle
stepList lst =
    List.map step lst


{-| -}
compareParticles : Particle -> Particle -> Order
compareParticles p q =
    let
        a =
            p.position

        b =
            q.position
    in
    if a.x < b.x then
        LT
    else if a.x > b.x then
        GT
    else if a.y < b.y then
        LT
    else if a.y > b.y then
        GT
    else if a.z < b.z then
        LT
    else if a.z > b.z then
        GT
    else
        EQ


areParticlesEqual : Particle -> Particle -> Bool
areParticlesEqual a b =
    compareParticles a b == EQ


{-| Remove all particles involved in a collision, and return the remaining ones
-}
removeCollidedParticles : List Particle -> List Particle
removeCollidedParticles lst =
    let
        sorted =
            List.sortWith compareParticles lst

        grouped =
            List.Extra.groupWhile areParticlesEqual sorted

        filtered =
            List.filter (\lst -> List.length lst == 1) grouped
    in
    List.concat filtered


{-| Compute the next step for a given particle system
-}
stepSystem : ParticleSystem -> Bool -> ParticleSystem
stepSystem system removeColliders =
    let
        newParticles =
            if removeColliders then
                removeCollidedParticles (stepList system.particles)
            else
                stepList system.particles

        newClosestToOrigin =
            findClosestToOrigin newParticles

        newIterationsSinceLastCTOChange =
            case ( newClosestToOrigin, system.closestToOrigin ) of
                ( Nothing, _ ) ->
                    0

                ( _, Nothing ) ->
                    0

                ( Just new, Just old ) ->
                    if new.index == old.index then
                        system.iterationsSinceLastCTOChange + 1
                    else
                        0
    in
    { particles = newParticles
    , closestToOrigin = newClosestToOrigin
    , iteration = system.iteration + 1
    , iterationsSinceLastCTOChange = newIterationsSinceLastCTOChange
    }


{-| Run the particle system until CTO hasn't changed for at least 10 iterations
-}
runSystem : ParticleSystem -> Bool -> ParticleSystem
runSystem system removeColliders =
    if system.iterationsSinceLastCTOChange > 10 then
        system
    else
        runSystem (stepSystem system removeColliders) removeColliders


{-| find particle closest to origin
-}
findClosestToOrigin : List Particle -> Maybe Particle
findClosestToOrigin particles =
    minimumBy (\p -> p.position.x * p.position.x + p.position.y * p.position.y + p.position.z * p.position.z) particles


{-| Initialize a particle list
-}
initParticleList : List Particle -> ParticleSystem
initParticleList particles =
    { particles = particles
    , closestToOrigin = findClosestToOrigin particles
    , iteration = 0
    , iterationsSinceLastCTOChange = 0
    }


type alias Model =
    { system : ParticleSystem
    , removeColliders : Bool
    }
