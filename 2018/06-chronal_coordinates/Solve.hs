-- required for compile-time check of regexes
{-# LANGUAGE QuasiQuotes #-}
 
module Solve where

import Data.Function
import Data.List
import Text.RE.Replace
import Text.RE.TDFA.String

type Instructions = String
type Grid = [String]

{-
data Coordinates = MkCoordinates {
  x :: Int,
  y :: Int
}  deriving (Show, Eq)
-}
type Coordinates = (Int, Int)

parseCoordinates :: String -> Coordinates
--parseCoordinates line = MkCoordinates x y 
parseCoordinates line = (x,y) 
  where regex = [re|^${x}([0-9]+), ${y}([0-9]+)|]
        (_, _, _, groups) = line =~ regex :: (String, String, String, [String]) -- before, match, after, [groups]
        x = read (groups !! 0) :: Int
        y = read (groups !! 1) :: Int

parseCoordinatesList :: String -> [Coordinates]
parseCoordinatesList input = map parseCoordinates (lines input)

-- build the grid: parse the list of coordinates, and compute the nearest coordinates for each square
buildGrid :: Instructions -> Grid
buildGrid input = grid
  where coordinatesList = parseCoordinatesList input
        max_x = fst $ last $ sortBy (compare `on` fst) coordinatesList
        max_y = fst $ last $ sortBy (compare `on` snd) coordinatesList
        line = concat $ take (max_x + 1) (repeat ".")
        grid = take (max_y + 1) (repeat line)

solveI = undefined

solveII = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    putStrLn $ show $ solveII input 
#endif

