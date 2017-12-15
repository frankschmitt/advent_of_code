module StreamProcessing where

-- given the (remaining) input, a nesting depth, and a partial score so far, compute the overall score
score' :: [Char] -> Int -> Int -> Int
score' "" _ n = n
score' (x:xs) depth n = 
  case x of
    '{' -> score' xs (depth + 1) n
    '}' -> score' xs (depth - 1) (n + depth) -- we update the score on closing brackets
    _   -> score' xs depth n 
 
-- compute the score for the given input string
score :: [Char] -> Int
score input = score' input 0 0 

