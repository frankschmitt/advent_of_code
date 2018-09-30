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
        ]
