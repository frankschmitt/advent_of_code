module SolveTest where

import Data.List
import Test.HUnit
import Solve

testSolvePartI  = test [ "solve for sample input" ~: 2 ~=? solveI "+1\n-2\n+3\n"
                       , "parses input" ~: [1,-2,3]    ~=? parseInput "+1\n-2\n+3\n"
                       ]

tests = TestList [ testSolvePartI
                 ] 

main = do runTestTT tests

