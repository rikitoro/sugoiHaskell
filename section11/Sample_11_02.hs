module Sample_11_02 where

data CMaybe a = CNothing | CJust Int a deriving (Show, Eq)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)