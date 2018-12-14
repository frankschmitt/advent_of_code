module SolveTest where

import Data.List
import Test.HUnit
import Solve

sampleInput = unlines [
  "[1518-11-01 00:00] Guard #10 begins shift",
  "[1518-11-01 00:05] falls asleep",
  "[1518-11-01 00:25] wakes up",
  "[1518-11-01 00:30] falls asleep",
  "[1518-11-01 00:55] wakes up",
  "[1518-11-01 23:58] Guard #99 begins shift",
  "[1518-11-02 00:40] falls asleep",
  "[1518-11-02 00:50] wakes up",
  "[1518-11-03 00:05] Guard #10 begins shift",
  "[1518-11-03 00:24] falls asleep",
  "[1518-11-03 00:29] wakes up",
  "[1518-11-04 00:02] Guard #99 begins shift",
  "[1518-11-04 00:36] falls asleep",
  "[1518-11-04 00:46] wakes up",
  "[1518-11-05 00:03] Guard #99 begins shift",
  "[1518-11-05 00:45] falls asleep",
  "[1518-11-05 00:55] wakes up"
  ]

testSolveThem  = test [ "solves part I" ~: 240 ~=? solveI sampleInput
                      , "solves part II" ~: 1 ~=? solveII sampleInput
                      , "parses wake up" ~: (MkEvent 55 WakeUp) ~=? (parseLine "[1518-11-05 00:55] wakes up")  
                      , "parses falls asleep" ~: (MkEvent 45 FallsAsleep) ~=? (parseLine "[1518-11-05 00:45] falls asleep")  
                      , "parses begins shift" ~: (MkEvent 0 (BeginsShift 10)) ~=? (parseLine "[1518-11-01 00:00] Guard #10 begins shift")  
                      ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

