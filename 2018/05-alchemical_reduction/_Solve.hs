module SolveTest where

import Data.List
import Test.HUnit
import Solve

sampleInput = "dabAcCaCBAcCcaDA"

testSolveThem  = test [ "solves part I" ~: 10 ~=? solveI(sampleInput)
                      , "solves part II" ~: 1 ~=? solveII
                      ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

