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

data Tree a = E | Leaf a | Join (Tree a) (Tree a) 
    deriving Show

mapreduce _ _ b E = b
mapreduce mf rf b (Leaf a) = mf a
mapreduce mf rf b (Join l r) = let (left, right) = mapreduce mf rf b l ||| mapreduce mf rf b r
                                in rf left right

cuaturpa v = let au = max v 0
             in (au, au, au, v)

reduche (m, p, s, t) (m', p', s', t') = (maximum [m, m', (s + p')], max p (t + p'), max s' (t' + s), t + t')

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = let (a, b, c, d) = (mapreduce cuaturpa reduche (0, 0, 0, 0) t)
         in a

tree2 = Join (Join (Join (Leaf 2) (Leaf (-3))) (Join (Leaf 5) (Leaf 7))) (Join (Join (Leaf (-10)) (Leaf 8)) (Join (Leaf (-8)) (Leaf 9)))

t :: Tree Int
t = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)

t2 :: Tree Int
t2 = Join (Join (Join (Leaf 100) (Leaf 110)) (Join (Leaf 90) (Leaf 80))) (Leaf 150)

isE E = True
isE _ = False

sufijos :: Tree Int -> Tree (Tree Int)
sufijos t = suf t E
            where
                suf (Leaf _) a = Leaf a
                suf (Join l r) a = let (l', r') = if isE a 
                                                  then (suf l r, suf r a) 
                                                  else (suf l (Join r a)) ||| (suf r a) 
                                    in Join l' r'

conSufijos t = let sub = sufijos t 
                in cs t sub
                    where
                        cs (Leaf v) (Leaf s) = Leaf (v, s)
                        cs (Join l r) (Join l' r') = let (left, right) = cs l l' ||| cs r r' 
                                                     in Join left right

reduce f b E = b
reduce f b (Leaf a) = a
reduce f b (Join l r) = let (l', r') = reduce f b l ||| reduce f b r
                        in f l' r'

maxT :: Tree Int -> Int
maxT t = reduce max 0 t 

maxAll t = mapreduce maxT max 0 t

mejorGanancia t = let conS = conSufijos t
                      bigger = mapreduce fsp max 0 conS
                  in bigger
                    where 
                        fsp (a, arb) = (maxT arb) - a