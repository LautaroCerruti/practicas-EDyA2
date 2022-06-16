data Tree a = N (Tree a) a (Tree a) | L a | E deriving Show

t = N (N (L 9) 3 (L 7)) 1 (N (N E 8 (N (L 12) 10 (N (L 20) 15 (L 18)))) 5 E)

inorder :: Tree a -> [a]
inorder E = []
inorder (L a) = [a]
inorder (N l a r) = (inorder l) ++ [a] ++ (inorder r)

size :: Tree a -> Int
size E = 0
size (L _) = 1
size (N l _ r) = 1 + size l + size r

subAux :: Ord a => Tree a -> Int -> Int -> Int -> Tree a
subAux E _ _ _ = E
subAux l@(L _) i j pv = let pos = pv + 1 in if pos >= i && pos <= j then l else E
subAux (N l a r) i j pv = let pos = pv + 1 + size l
                          in if pos >= i && pos <= j then (N (subAux l i j pv) a (subAux r i j pos)) else (if pos < i then (subAux r i j pos) else (subAux l i j pv))

subSequence :: Ord a => Tree a -> Int -> Int -> Tree a
subSequence t i j = subAux t i j 0

-- inorder t =  [9,3,7,1,8,12,10,20,15,18,5]