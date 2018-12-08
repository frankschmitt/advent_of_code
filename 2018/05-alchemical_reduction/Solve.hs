module Solve where

import Data.Char
import qualified Debug.Trace as DT


reduce :: String -> String
reduce inp = case inp of
  []  -> [] 
  [x] -> [x]
  x : y : ys -> if x == y then [x] ++ (reduce (y : ys))
                else if toUpper(x) == toUpper(y) then reduce ys
                else [x] ++ reduce (y : ys)    

fullyReduce :: String -> String
fullyReduce input = 
  let 
     reduced = reduce(input) in 
  if length(reduced) == length(input) then reduced
  else DT.trace ("fullyReduce, len: " ++ show (length reduced)) (fullyReduce(reduced))

solveI :: String -> Int 
solveI input = (length $ DT.trace ("fullyReduced: >>" ++ s ++ "<<") s) - 1
   where s = fullyReduce input 

solveII = undefined

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
--    putStrLn $ show $ solveII input 
#endif

