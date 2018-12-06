module SolveTest where

import Data.List
import Test.HUnit
import Solve

sampleInput = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n"
sampleClaim = "#1 @ 1,3: 4x5"


testSolveThem  = test [ "solves part I" ~: 4 ~=? solveI(sampleInput)
                      , "solves part II" ~: 1 ~=? solveII(sampleInput)
                      , "parses Claim" ~: Right (MkClaim 1 (MkRectangle (MkPoint 1 3) (MkDimensions 4 5))) ~=? (regularParse claimParser sampleClaim)
                      ]

tests = TestList [ testSolveThem
                 ] 

main = do runTestTT tests

