module Solve where

import qualified Data.Set as Set

frequency :: [Int] -> Int
frequency = foldl (+) 0 

toInt :: String -> Int
toInt s = read s

parseInput :: String -> [Int]
parseInput input =
  map toInt x2
  where x1 = filter (\ch -> ch /= '+') input 
        x2 = lines x1

solveI :: String -> Int
solveI input = frequency $ parseInput input

-- recursive helper for part II
-- arguments: list of frequency diffs, frequencies seen so far, current frequency
-- returns: first frequency that repeats
solveII' :: [Int] -> Set.Set Int -> Int -> Int 
solveII' (x:xs) seen currentFreq =
  let 
    nextFreq = currentFreq + x
  in 
    if Set.member nextFreq seen then nextFreq
    else solveII' xs (Set.insert nextFreq seen) nextFreq

solveII :: String -> Int 
solveII input = solveII' repeatFrequencies Set.empty 0  
  where repeatFrequencies = cycle $ parseInput input

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input  
    putStrLn $ show $ solveII input  
#endif

