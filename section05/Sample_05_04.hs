module Sample_05_04 where

import Sample_05_03

numLongChains' :: Int
numLongChains' = length $ filter (\xs -> length xs > 15) $ map chain [1..100]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x