module Duet(main) where

import Test.Prop

-- square an integer
square :: Int -> Int
square x = x*x


main = square 9

-- reverse a list
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

-- Properties
revNull = rev []      -=- []
rev123  = rev [1,2,3] -=- [3,2,1]
revRevIsId xs = rev (rev xs) -=- xs
