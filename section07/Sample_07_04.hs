module Sample_07_04 where

data Car = Car { company :: String, model :: String, year :: Int } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y})
  = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

----------
data Vector a = Vector a a a deriving (Show, Eq)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m) 