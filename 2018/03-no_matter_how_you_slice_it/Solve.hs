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

-- MAIN routines
solveI :: String -> Int
solveI input = 7
  where claims = lines input
        m = matrix 1001 1001 $ \(i,j) -> 0 -- 1001x1001, all elements are 0

solveII :: String -> Int
solveII = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    putStrLn $ show $ solveII input 
#endif

