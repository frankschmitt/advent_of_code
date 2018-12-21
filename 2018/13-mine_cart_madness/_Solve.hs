module SolveTest where

import Data.List
import Test.HUnit
import Solve

testSolveThem  = test [ "solves part I" ~: 1 ~=? solveI
                      , "solves part II" ~: 1 ~=? solveII
                      ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

