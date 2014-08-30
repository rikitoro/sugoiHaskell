module Sample_07_02_1 where

data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
             deriving (Show, Eq)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
