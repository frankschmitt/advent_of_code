module SolveTest where

import Data.List
import Test.HUnit
import Solve

-- we changed the ordering of the instructions since the default ordering matches the solution and is therefore not a good test case
sampleInput = unlines [
  "Step A must be finished before step B can begin.",
  "Step A must be finished before step D can begin.",
  "Step C must be finished before step A can begin.",
  "Step C must be finished before step F can begin.",
  "Step B must be finished before step E can begin.",
  "Step D must be finished before step E can begin.",
  "Step F must be finished before step E can begin."
  ]

testSolveThem  = test [ "solves part I" ~: "CABDFE" ~=? (solveI sampleInput)
                      , "solves part II" ~: 1 ~=? solveII
                      ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

