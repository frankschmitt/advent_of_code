module StreamProcessingTest where

import Data.List
import Test.HUnit
import StreamProcessing

testScoreForSingleBracketPair  = test [ "score for single pair of brackets" ~: (1,0) ~=? score "{}" ]

testScoreForNestedBrackets  = test [ "score for nested brackets" ~: (6,0) ~=? score "{{{}}}" ]

testScoreForSequenceOfBrackets  = test [ "score for sequence of brackets" ~: (5,0) ~=? score "{{},{}}" ]

testScoreWithJunk = test [ "score with junk" ~: (1,4) ~=? score "{<a>,<a>,<a>,<a>}"]

testScoreWithMoreJunk = test [ "score with more junk" ~: (9,8) ~=? score "{{<ab>},{<ab>},{<ab>},{<ab>}}" ]

testScoreWithEscapedExclamation = test [ "score with escaped exclamation" ~: (9,0) ~=? score "{{<!!>},{<!!>},{<!!>},{<!!>}}" ]

testScoreWithEscapedClosingBrackets = test [ "score with escaped closing brackets" ~: (3,17) ~=? score "{{<a!>},{<a!>},{<a!>},{<ab>}}" ]

tests = TestList [ testScoreForSingleBracketPair
                 , testScoreForNestedBrackets 
                 , testScoreForSequenceOfBrackets
                 , testScoreWithJunk
                 , testScoreWithMoreJunk
                 , testScoreWithEscapedExclamation
                 , testScoreWithEscapedClosingBrackets
                 ] 

main = do runTestTT tests

