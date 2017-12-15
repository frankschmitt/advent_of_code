module StreamProcessing where

-- read garbage from input, discard it, and return the remaining string and the number of characters that were removed (minus canceled characters)
removeGarbage :: ([Char], Int) -> ([Char], Int)
removeGarbage (input,n) = 
  case input of
    '>' : xs -> (xs, n)
    '!' : x : xs -> removeGarbage (xs, n)
    x : xs -> removeGarbage (xs, n+1)

-- given the (remaining) input, a nesting depth, a partial score so far, and the number of removed characters so far, compute the overall score and the number of removed characters overall
score' :: [Char] -> Int -> (Int, Int) -> (Int, Int)
score' "" _ (s,r) = (s,r) 
score' (x:xs) depth (s,r) = 
  case x of
    '{' -> score' xs (depth + 1) (s,r) 
    '}' -> score' xs (depth - 1) (s + depth, r) -- we update the score on closing brackets
    '<' -> 
       let (xs', r') = removeGarbage (xs, r)
        in score' xs' depth (s, r')
    _   -> score' xs depth (s,r) 
 
-- compute the score and number of removed characters for the given input string
score :: [Char] -> (Int,Int)
score input = score' input 0 (0,0) 

#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ score input  -- (16021, 7685)
#endif
