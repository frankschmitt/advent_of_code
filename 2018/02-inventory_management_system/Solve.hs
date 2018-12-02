module Solve where

import Data.List
import Data.Maybe

-- check whether a given sorted string (= character list)  has characters that appear 
--   exactly twice or thrice
chk2and3 :: [String] -> (Int, Int)
chk2and3 s1 = foldl countIt (0,0) s1 
  where countIt (cnt2, cnt3) s = case (cnt2, cnt3, length(s)) of
                                    (0, _, 2) -> (1, cnt3)
                                    (_, 0, 3) -> (cnt2, 1)
                                    _         -> (cnt2, cnt3)

-- compute the 2- and 3-score for an input string
score :: String -> (Int, Int)
-- just for fun, we use point-free notation here :-)
score = chk2and3 . group . sort


-- compute the 2- and 3-score for each input word (returns a 2-elem tuple), sum the list of tuples, and multiply the tuple elements
solveI :: String -> Int 
solveI input = (fst scoreSum) * (snd scoreSum)
  where scoreSum = foldl (\(agg2, agg3) (cnt2, cnt3) -> (agg2 + cnt2, agg3 + cnt3)) (0,0) $ map score $ lines input 


-- find the common string between s1 and s2, wrapped in a Maybe
--   returns Just s iff the strings are similar (i.e. their common string is n-1 characters long)
commonString :: String -> String -> Maybe String
commonString s1 s2 = if length(cs)== length(s1) - 1 then Just cs else  Nothing
  where helper s1' s2' = concat $ zipWith (\a b -> if a == b then charToString a else "") s1' s2'
        cs = helper s1 s2
        charToString c = [c]

-- find the similar string to the given input string 
findSimilar :: String -> [String] -> Maybe String
findSimilar _ [] = Nothing
findSimilar s (x:xs) = case cs of Just _  -> cs
                                  Nothing -> findSimilar s xs
   where cs = commonString s x

-- find the first pair of strings that differ in exactly one element, and return the common string
solveII' :: [String] -> String
solveII' [] = "" -- return empty string for empty input list; this should never happen
solveII' (x:xs) = case findSimilar x xs of Just s  -> s
                                           Nothing -> solveII' xs

-- find the first pair of input strings that differ in exactly one character, and return the common letters
solveII :: String -> String
solveII input = solveII' $ lines input

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI input
    putStrLn $ show $ solveII input
#endif
