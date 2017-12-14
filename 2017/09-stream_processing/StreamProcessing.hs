module StreamProcessing where

-- given the (remaining) input and a partial score so far, compute the overall score
score' :: [Char] -> Int -> Int
score' "" n = n
score' (x:xs) n = 
  case x of
    '{' -> score' xs (  n + 1)
    _   -> score' xs n
 
-- compute the score for the given input string
score :: [Char] -> Int
score input = score' input 0 

