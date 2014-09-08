module Sample_12_03 where

import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

lengthCompare1 :: String -> String -> Ordering
lengthCompare1 x y = (length x `compare` length y) `mappend`
  (vowels x `compare` vowels y) `mappend`
  (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
