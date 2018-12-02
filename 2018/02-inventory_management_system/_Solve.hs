module SolveTest where

import Data.List
import Test.HUnit
import Solve

sampleInputI  = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab\n"
sampleInputII = "abcde\nfghij\nklmno pqrst\nfguij\naxcye\nwvxyz\n"
testSolveThem   = test [ "solveI for sample input" ~: 12 ~=? solveI(sampleInputI)
                       , "score a word with no 2 and no 3" ~: (0,0) ~=? score("abcdef")
                       , "score a word with one non-consecutive 2 and no 3" ~: (1,0) ~=? score("abcdea")
                       , "score a word with two 2s and one 3" ~: (1,1) ~=? score("ababccc")
                       , "solveII for sample input" ~: "fgij" ~=? solveII(sampleInputII)
                       ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

