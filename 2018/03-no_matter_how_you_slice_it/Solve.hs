module Solve where

import Data.Matrix
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

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

claim1 :: Claim
claim1 = MkClaim 1 (MkRectangle (MkPoint 2 3) (MkDimensions 4 5)) 
-- MAIN routines
--
-- create a 1001x1001 matrix for the given claim
-- the matrix elements equal the claim's index in the claim's area 
-- and 0 everywhere else
toMatrix :: Claim -> Matrix Int
toMatrix c = foldl (\m (i,j) -> setElem (index c) (i,j) m) (zero 1001 1001) indices
--   where indices = [(1,1)]
  where r = rectangle c
        x' = x $ upperLeft r
        y' = y $ upperLeft r
        width' = width $ dimensions r
        height' = height $ dimensions r
        indices = [(i,j) | i <- [(x'+1) .. (x' + width')],
                           j <- [(y'+1) .. (y' + height')]]


-- apply the given claim to the given matrix
-- we mark each square with claim's index; if the square is already occupied,
-- it is marked with a -1 instead
applyClaim :: Claim -> Matrix Int -> Matrix Int
applyClaim c m = elementwise combine m (toMatrix c) 
  where combine oldElem claimElem = case (oldElem, claimElem) of
         (o1, 0)  -> o1 -- square unaffected by new claim -> keep old claim
         (0 , c1) -> c1 -- square was not occupied -> set to new claim
         (_ , _ ) -> -1 -- square was occupied, mark with -1 

-- apply the given claims to the given matrix
applyClaims :: [Either ParseError Claim ] -> Matrix Int -> Matrix Int
applyClaims [] m = m
applyClaims (x:xs) m = 
  case x of
    Right x' -> applyClaims xs (applyClaim x' m)
    Left _   -> applyClaims xs m -- leave m unchanged for erroneous claim

countClashes :: Matrix Int -> Int
countClashes m = length $ filter (\x -> x == -1) $ toList m

solveI :: String -> Int
solveI input = countClashes m2
  where claims = map (\line -> regularParse claimParser line) $ lines input
        m = zero 1001 1001 -- 1001x1001, all elements are 0
        m2 = applyClaims claims m

solveII :: String -> Int
solveII = undefined

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    --putStrLn $ show $ solveII input 
#endif
