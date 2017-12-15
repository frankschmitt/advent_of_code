module StreamProcessing where

-- read garbage from input, discard it, and return the remaining string
removeGarbage :: [Char] -> [Char]
removeGarbage input = 
  case input of
    '>' : xs -> xs
    '!' : x : xs -> removeGarbage xs
    x : xs -> removeGarbage xs

-- given the (remaining) input, a nesting depth, and a partial score so far, compute the overall score
score' :: [Char] -> Int -> Int -> Int
score' "" _ n = n
score' (x:xs) depth n = 
  case x of
    '{' -> score' xs (depth + 1) n
    '}' -> score' xs (depth - 1) (n + depth) -- we update the score on closing brackets
    '<' -> score' (removeGarbage xs) depth n
    _   -> score' xs depth n 
 
-- compute the score for the given input string
score :: [Char] -> Int
score input = score' input 0 0 

main = do
    input <- readFile "input.txt"
    putStrLn $ show $ score input  -- 16021

