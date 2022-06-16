import Par

data BTree a = Empty | Node Int (BTree a) a (BTree a)
    deriving Show

inorder Empty = []
inorder (Node _ l a r) = let (left, right) = inorder l ||| inorder r
                        in left ++ [a] ++ right

size Empty = 0
size (Node s _ _ _) = s

nths (Node _ l v r) i = let pos = 1 + size l 
                        in if pos > i then nths l i else (if pos < i then nths r (i - pos) else v)

cons e Empty = Node 1 Empty e Empty
cons e (Node s l a r) = Node (s+1) (cons e l) a r

intDivCeil n d = -(div (-n) d)
intDivFloor n d = div n d

tabulate :: (Int->a)->Int->BTree a
tabulate f n = tabulateAux n 0
    where
        tabulateAux 0 _ = Empty
        tabulateAux q o = let pos = 1+(intDivCeil (q-1) 2)+o
                              ((l, v), r) = (tabulateAux (intDivCeil (q-1) 2) o ||| f pos) ||| tabulateAux (intDivFloor (q-1) 2) pos
                          in Node q l v r

mapT f Empty = Empty
mapT f (Node s l a r) = let ((le, v), ri) = (mapT f l ||| f a) ||| mapT f r
                        in Node s le v ri

takeT 0 _ = Empty
takeT _ Empty = Empty
takeT n (Node _ l a r) = let pos = 1 + size l
                             rightt = if pos < n then takeT (n-pos) r else Empty
                        in if pos > n then takeT n l else Node (1 + size l + size rightt) l a rightt 

dropT _ Empty = Empty
dropT 0 a = a
dropT n (Node _ l a r) = let pos = 1 + size l
                             leftt = if pos > n then dropT n l else Empty
                        in if pos < n then dropT (n-pos) r else if pos == n then r else Node (1 + size leftt + size r) leftt a r 
