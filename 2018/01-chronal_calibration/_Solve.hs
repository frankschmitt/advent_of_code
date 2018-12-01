module SolveTest where

import Data.List
import Test.HUnit
import Solve

testSolvePartI  = test [ "solves part I" ~: 1 ~=? solve
                       , "frequency for sample input" ~: 2 ~=? frequency [1, -2, 3] 
                       , "parses input" ~: 2 ~=? frequencyStr "+1\n-2\n+3\n"
                       ]

tests = TestList [ testSolvePartI
                 ] 

main = do runTestTT tests

