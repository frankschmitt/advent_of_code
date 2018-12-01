module SolveTest where

import Data.List
import Test.HUnit
import Solve

testSolvePartI  = test [ "solves part I" ~: 1 ~=? solve  ]

tests = TestList [ testSolvePartI
                 ] 

main = do runTestTT tests

