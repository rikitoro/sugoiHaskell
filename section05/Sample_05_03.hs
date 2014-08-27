module Sample_05_03 where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      lager = filter (> x) xs
  in quicksort smallerOrEqual ++ [x] ++ quicksort lager


largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n  = n : chain (n `div` 2)
  | odd n   = n : chain (n * 3 + 1)
