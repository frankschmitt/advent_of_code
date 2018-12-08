module Solve where

import Data.Char
import qualified Debug.Trace as DT
import Data.List

reduce :: String -> String
reduce inp = case inp of
  []  -> [] 
  [x] -> if x == '\n' then [] else [x] -- omit trailing newline
  x : y : ys -> if ((x /= y) && (toUpper(x) == toUpper(y))) then reduce ys -- reduce pair
                else [x] ++ reduce (y : ys)    

fullyReduce :: String -> String
fullyReduce input = 
  let 
     reduced = reduce(input) in 
  if length(reduced) == length(input) then reduced
  -- else DT.trace ("fullyReduce, len: " ++ show (length reduced)) (fullyReduce(reduced))
  else fullyReduce reduced

removeAll :: String -> Char -> String
removeAll s c = filter (\c' -> c' /= c && c' /= toUpper(c)) s

solveI :: String -> Int 
solveI = length . fullyReduce

solveII :: String -> Int
solveII input = minimum $ map (\ch -> length $ fullyReduce $ removeAll input ch) ['a' .. 'z']

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    putStrLn $ show $ solveII input 
#endif
