module Tests exposing (..)

import Expect
import Main
import Test exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "adding a vector with v(1,2,3) should increase it by x=1, y=2, z=3" <|
            \_ ->
                Expect.equal { x = 5, y = 7, z = 9 } (Main.move { x = 1, y = 2, z = 3 } { x = 4, y = 5, z = 6 })
        , test "moving a particle should update its position and velocity" <|
            \_ ->
                Expect.equal { index = 1, position = { x = 4, y = 0, z = 0 }, velocity = { x = 1, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }
                    (Main.step { index = 1, position = { x = 3, y = 0, z = 0 }, velocity = { x = 2, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } })
        ]
