module Sample_11_01 where

rep3 :: (Functor f) => f a -> f [a]
rep3 = fmap $ replicate 3

