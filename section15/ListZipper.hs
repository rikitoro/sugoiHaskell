type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b:bs) = (b:xs, bs)