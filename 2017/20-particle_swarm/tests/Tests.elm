module Tests exposing (..)

import Expect
import Main
import Test exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!
-- test data; p0_1: particle 0 in 1st iteration


p0_0 =
    { index = 0, position = { x = 3, y = 0, z = 0 }, velocity = { x = 2, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }


p0_1 =
    { index = 0, position = { x = 4, y = 0, z = 0 }, velocity = { x = 1, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }


p1_0 =
    { index = 1, position = { x = 4, y = 0, z = 0 }, velocity = { x = 0, y = 0, z = 0 }, acceleration = { x = -2, y = 0, z = 0 } }


p1_1 =
    { index = 1, position = { x = 2, y = 0, z = 0 }, velocity = { x = -2, y = 0, z = 0 }, acceleration = { x = -2, y = 0, z = 0 } }


particles_0 =
    [ p0_0, p1_0 ]


particles_1 =
    [ p0_1, p1_1 ]


all : Test
all =
    describe "A Test Suite"
        [ test "adding a vector with v(1,2,3) should increase it by x=1, y=2, z=3" <|
            \_ ->
                Expect.equal { x = 5, y = 7, z = 9 } (Main.move { x = 1, y = 2, z = 3 } { x = 4, y = 5, z = 6 })
        , test "step for a particle should update its position and velocity" <|
            \_ ->
                Expect.equal p0_1 (Main.step p0_0)
        , test "step for a particle list should update all positions and velocities" <|
            \_ ->
                Expect.equal particles_1 (Main.stepList particles_0)
        ]
