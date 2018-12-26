module SolveTest where

import Data.List
import Test.HUnit
import Solve

sampleInput = unlines [
  "1, 1",
  "1, 6",
  "8, 3",
  "3, 4",
  "5, 5",
  "8, 9"
  ]

sampleGrid = [
  "aaaaa.cccc",
  "aAaaa.cccc",
  "aaaddecccc",
  "aadddeccCc",
  "..dDdeeccc",
  "bb.deEeecc",
  "bBb.eeee..",
  "bbb.eeefff",
  "bbb.eeffff",
  "bbb.ffffFf"
  ]

testSolveThem  = test [ "solves part I" ~: 1 ~=? solveI
                      , "solves part II" ~: 1 ~=? solveII
                      , "builds grid" ~: sampleGrid ~=? (buildGrid sampleInput)
                      , "parses coords" ~: (353, 177) ~=? (parseCoordinates "353, 177")
                      ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

