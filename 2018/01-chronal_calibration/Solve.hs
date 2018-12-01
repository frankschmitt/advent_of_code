module Solve where

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

solve = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI input  -- (16021, 7685)
#endif

