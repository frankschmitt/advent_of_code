module Solve where


solveI = undefined

solveII = undefined


#if defined(STANDALONE)
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ solveI  input 
    putStrLn $ show $ solveII input 
#endif

