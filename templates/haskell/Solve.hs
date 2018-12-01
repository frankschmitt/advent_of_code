module Solve where


solve = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ score input  -- (16021, 7685)
#endif

