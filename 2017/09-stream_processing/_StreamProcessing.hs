module StreamProcessingTest where

import Data.List
import Test.HUnit
import StreamProcessing

testScoreForSingleBracketPair  = test [ "score for single pair of brackets" ~: 1 ~=? score "{}" ]

testScoreForNestedBrackets  = test [ "score for nested brackets" ~: 6 ~=? score "{{{}}}" ]

testScoreForSequenceOfBrackets  = test [ "score for sequence of brackets" ~: 5 ~=? score "{{},{}}" ]

testScoreWithJunk = test [ "score with junk" ~: 1 ~=? score "{<a>,<a>,<a>,<a>}"]

-- {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
--{{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
--{{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.

tests = TestList [ testScoreForSingleBracketPair
                 , testScoreForNestedBrackets 
                 , testScoreForSequenceOfBrackets
                 , testScoreWithJunk
                 ] 

main = do runTestTT tests

