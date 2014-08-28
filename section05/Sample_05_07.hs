module Sample_05_07 where

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' =  sum . takeWhile (<10000) . filter odd $ map (^2) [1..]