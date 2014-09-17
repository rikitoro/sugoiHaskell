data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

freeTree :: Tree Char
freeTree =
    Node 'P'
             (Node 'O'
                       (Node 'L' 
                                 (Node 'N' Empty Empty)
                                 (Node 'T' Empty Empty)
                       )
                       (Node 'T' 
                                 (Node 'S' Empty Empty)
                                 (Node 'A' Empty Empty)
                       )

             )
             (Node 'L'
                       (Node 'W' 
                                 (Node 'C' Empty Empty)
                                 (Node 'R' Empty Empty)
                       )
                       (Node 'A' 
                                 (Node 'A' Empty Empty)
                                 (Node 'C' Empty Empty)
                       )

             )


changeToP' :: Tree Char -> Tree Char
changeToP' (Node x l (Node y (Node _ m n) r)) =
    Node x l (Node y (Node 'P' m n) r)


data Direction = L | R deriving Show
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

(-:) :: a -> (a -> b) -> b
x -: f = f x

data Crumb a = LeftCrumb a (Tree a)
    | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]


type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing
