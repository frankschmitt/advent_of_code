module StreamProcessingTest where

import Data.List
import Test.HUnit
import StreamProcessing

testScoreForSingleBracketPair  = test [ "score for single pair of brackets" ~: 1 ~=? score "{}" ]
testScoreForNestedBrackets  = test [ "score for nested brackets" ~: 6 ~=? score "{{{}}}" ]

tests = TestList [ testScoreForSingleBracketPair
                 , testScoreForNestedBrackets 
                 ] 

main = do runTestTT tests

