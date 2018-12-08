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
-- myClaim = MkClaim 1 (MkRectangle (MkPoint 1 1) (MkDimensions 5 5) )
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

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

-- TEST DATA
-- claim1 :: Claim
-- claim1 = MkClaim 1 (MkRectangle (MkPoint 2 3) (MkDimensions 4 5)) 
claim1 = MkClaim 1 (MkRectangle (MkPoint 509 796) (MkDimensions 18 15))
claim2 = MkClaim 2 (MkRectangle (MkPoint 724 606) (MkDimensions 23 15))
claim3 = MkClaim 3 (MkRectangle (MkPoint 797 105) (MkDimensions 10 13))

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
    --Right x' -> applyClaims xs (DT.trace ("applyClaim " ++ (show $ index x')) (applyClaim x' m))
    Left _   -> applyClaims xs m -- leave m unchanged for erroneous claim

countClashes :: Matrix Int64 -> Int
countClashes m = length $ filter (\x -> x == -1) $ toList $ flatten m 

solveI :: String -> Int
--solveI input = DT.trace "counting clashes" (countClashes m2)
solveI input = countClashes m2 
  where claims = map (\line -> regularParse claimParser line) $ lines input
        m = (1001><1001) (repeat 0) -- 1001x1001, all elements are 0
        m2 = applyClaims claims m
        --m2 = DT.trace ("applying claims " ++ (show $ length claims)) (applyClaims claims m)

solveII :: String -> Int
solveII = undefined

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    --putStrLn $ show $ solveII input 
#endif
