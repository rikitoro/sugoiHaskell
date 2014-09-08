module Sample_12_01 where

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a, b) } deriving (Eq, Show)
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool' = CoolBool' { getCoolBool' :: Bool}
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"