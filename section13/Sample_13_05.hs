module Sample_13_05 where

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

-------------

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing


landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left,right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = do
  start  <- return (0, 0)
  first  <- landLeft 2 start
  second <- landRight 3 first
  landLeft 1 second

routineF :: Maybe Pole
routineF = do
  start  <- return (0, 0)
  first  <- landLeft 2 start
  second <- landRight 6 first
  landLeft 1 second

routineB :: Maybe Pole
routineB = do
  start <- return (0, 0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x