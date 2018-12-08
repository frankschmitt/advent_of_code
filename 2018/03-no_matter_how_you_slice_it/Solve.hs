module Solve where

import Data.List
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import qualified Debug.Trace as DT
import Numeric.LinearAlgebra.Data
import Data.Int

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


-- TYPE DEFINITIONS
data Point = MkPoint {
  x :: Int,
  y :: Int
} deriving (Show, Eq)

data Dimensions = MkDimensions {
  width  :: Int,
  height :: Int
} deriving (Show, Eq)

data Rectangle = MkRectangle {
  upperLeft :: Point,
  dimensions :: Dimensions
} deriving (Show, Eq)
  
data Claim  = MkClaim {
    index :: Int,
    rectangle :: Rectangle
} deriving (Show, Eq)

-- PARSING
-- of course, we could simply use a regular expression, but I've always wanted to play 
-- around with the Parsec library for generating parsers, so bear with me :-)
numParser :: Parser Int
numParser = do
    n <- many1 digit
    return (read n)

pointParser :: Parser Point
pointParser = do
    x <- numParser
    void $ char ','
    y <- numParser
    return (MkPoint x y) 

dimensionsParser :: Parser Dimensions
dimensionsParser = do
  width <- numParser
  void $ char 'x'
  height <- numParser
  return (MkDimensions width height)

rectangleParser :: Parser Rectangle
rectangleParser = do
    p <- pointParser
    void $ char ':'
    void $ char ' '
    dim <- dimensionsParser
    return (MkRectangle p dim)
     
claimParser :: Parser Claim
claimParser = do
    void $ char '#'
    idx <- numParser
    void $ char ' '
    void $ char '@'
    void $ char ' '
    rect <- rectangleParser
    return (MkClaim idx rect)

-- helper function to apply a parser
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


-- MAIN routines
--
-- create an association list for the given claim
-- the association list contains a list of (x,y) coordinates plus the value (which equals the claim's index)
toAssocList :: Claim -> [((Int, Int), Int64)]
toAssocList c = map (\(i,j) -> ((i,j), (fromIntegral idx))) indices 
  where idx = index c 
        r = rectangle c
        x' = x $ upperLeft r
        y' = y $ upperLeft r
        width' = width $ dimensions r
        height' = height $ dimensions r
        indices = [(i,j) | i <- [(x'+1) .. (x' + width')],
                           j <- [(y'+1) .. (y' + height')]]


-- apply the given claim to the given matrix
applyClaim :: Claim -> Matrix Int64 -> Matrix Int64
applyClaim c m = accum m (\new old -> if old == 0 then new else -1) (toAssocList c)

-- apply the given list of claims to the given matrix
applyClaims :: [Either ParseError Claim ] -> Matrix Int64 -> Matrix Int64
applyClaims [] m = m
applyClaims (x:xs) m = 
  case x of
    Right x' -> applyClaims xs (applyClaim x' m)
    Left _   -> applyClaims xs m -- leave m unchanged for erroneous claim

-- count the elements in the given matrix that are equal to the given value
countElems :: Matrix Int64 -> Int64 -> Int
countElems m val = length $ filter (\x -> x == val) $ toList $ flatten m 

-- count the clashes (squares with overlapping claims)
countClashes :: Matrix Int64 -> Int
countClashes m = countElems m (-1)

-- prepare solution: parse input, build a 1001x1001 matrix, and apply all claims to it
prepSolution:: String -> ([Claim], Matrix Int64)
prepSolution input = (validClaims, applyClaims claims m)
  where claims = map (\line -> regularParse claimParser line) $ lines input
        m = (1001><1001) (repeat 0) -- 1001x1001, all elements are 0
        m2 = applyClaims claims m
        validClaims = map (\(Right x) -> x) $ filter (\x -> case x of Right _ -> True) claims

-- find the single claim that is intact
-- our approach is kind of brute force - for n claims, we check all matrix elements n times
findIntactClaim :: ([Claim], Matrix Int64) -> Claim
findIntactClaim (claims, m) = head $ filter (\c -> area c == countElems m (fromIntegral (index c))) claims
  where area c' = width (dimensions (rectangle c')) * height (dimensions (rectangle c'))

solveI :: String -> Int
solveI = countClashes . snd . prepSolution

solveII :: String -> Int
solveII = index . findIntactClaim . prepSolution 

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    putStrLn $ show $ solveII input 
#endif
