module StreamProcessingTest where

import Data.List
import Test.HUnit
import StreamProcessing

testScoreForSingleBracketPair  = test [ "score for single pair of brackets" ~: 1 ~=? score "{}" ]

testScoreForNestedBrackets  = test [ "score for nested brackets" ~: 6 ~=? score "{{{}}}" ]

testScoreForSequenceOfBrackets  = test [ "score for sequence of brackets" ~: 5 ~=? score "{{},{}}" ]

testScoreWithJunk = test [ "score with junk" ~: 1 ~=? score "{<a>,<a>,<a>,<a>}"]

testScoreWithMoreJunk = test [ "score with more junk" ~: 9 ~=? score "{{<ab>},{<ab>},{<ab>},{<ab>}}" ]

testScoreWithEscapedExclamation = test [ "score with escaped exclamation" ~: 9 ~=? score "{{<!!>},{<!!>},{<!!>},{<!!>}}" ]

testScoreWithEscapedClosingBrackets = test [ "score with escaped closing brackets" ~: 3 ~=? score "{{<a!>},{<a!>},{<a!>},{<ab>}}" ]

tests = TestList [ testScoreForSingleBracketPair
                 , testScoreForNestedBrackets 
                 , testScoreForSequenceOfBrackets
                 , testScoreWithJunk
                 , testScoreWithMoreJunk
                 , testScoreWithEscapedExclamation
                 , testScoreWithEscapedClosingBrackets
                 ] 

main = do runTestTT tests

