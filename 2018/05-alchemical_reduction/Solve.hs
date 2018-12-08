module Solve where

import Data.Char
import qualified Debug.Trace as DT
import Data.List

reduce :: String -> String
reduce inp = case inp of
  []  -> [] 
  [x] -> if x == '\n' then [] else [x] -- omit trailing newline
  --x : y : ys -> if x == y then [x] ++ (reduce (y : ys)) -- do not reduce same characters
  --              else if toUpper(x) == toUpper(y) then reduce ys -- reduce pair
  --              else [x] ++ reduce (y : ys)    
  x : y : ys -> if ((x /= y) && (toUpper(x) == toUpper(y))) then reduce ys -- reduce pair
                else [x] ++ reduce (y : ys)    

fullyReduce :: String -> String
fullyReduce input = 
  let 
     reduced = reduce(input) in 
  if length(reduced) == length(input) then reduced
  -- else DT.trace ("fullyReduce, len: " ++ show (length reduced)) (fullyReduce(reduced))
  else fullyReduce reduced

removeAllPairs :: String -> Char -> String
removeAllPairs inp ch = case inp of
  []  -> [] 
  [x] -> if x == '\n' then [] else [x] -- omit trailing newline
  x : y : ys -> if (toUpper(x) == toUpper(y) && toUpper(x) == ch) then removeAllPairs ys ch
                else [x] ++ removeAllPairs (y : ys) ch

removeAll :: String -> Char -> String
removeAll s c = filter (\c' -> c' /= c && c' /= toUpper(c)) s

solveI :: String -> Int 
solveI = length . fullyReduce

solveII input = minimum $ map (\ch -> length $ fullyReduce $ removeAll input ch) ['a' .. 'z']

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    putStrLn $ show $ solveII input 
#endif
