module SolveTest where

import Data.List
import Test.HUnit
import Solve

testSolve = test [ "solveI for sample input" ~: 10 ~=? solveII "+3\n+3\n+4\n-2\n-4\n"
                 , "solveII for sample input" ~: 2 ~=? solveI  "+1\n-2\n+3\n"
                 , "parses input" ~: [1,-2,3]    ~=? parseInput "+1\n-2\n+3\n"
                 ]

tests = TestList [ testSolve
                 ] 

main = do runTestTT tests

