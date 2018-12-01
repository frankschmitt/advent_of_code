module Solve where

frequency :: [Int] -> Int
frequency = foldl (+) 0 

toInt :: String -> Int
toInt s = read s

frequencyStr :: String -> Int
frequencyStr input =
  frequency xs 
  where x1 = filter (\ch -> ch /= '+') input 
        x2 = lines x1
        xs = map toInt x2

solve = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ frequencyStr input  -- (16021, 7685)
#endif

