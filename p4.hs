--1
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)
    deriving Show

completo :: a -> Int -> BinTree a
completo x 0 = EmptyB
completo x i = let t = completo x (i-1)
                in NodeB t x t

balanceado x 0 = EmptyB
balanceado x i | ((mod (i-1) 2) == 0) = let t = balanceado x (div (i-1) 2) in NodeB t x t
               | otherwise          = NodeB (balanceado x ((div (i-2) 2)+1)) x (balanceado x (div (i-2) 2))

--2
arb = NodeB (NodeB (NodeB EmptyB 1 (NodeB EmptyB 2 EmptyB)) 3 (NodeB EmptyB 4 EmptyB)) 5 (NodeB (NodeB EmptyB 6 (NodeB EmptyB 7 EmptyB)) 8 (NodeB EmptyB 9 EmptyB))

member :: Ord a => a -> BinTree a -> Bool
member a EmptyB = False
member a (NodeB l b r) | a == b = True
                       | a < b = member a l
                       | a > b = member a r

inorder :: BinTree a -> [a]
inorder EmptyB = []
inorder (NodeB l a r) = inorder l ++ [a] ++ inorder r

minimumBST :: BinTree a -> a
minimumBST (NodeB EmptyB a r) = a
minimumBST (NodeB l a r) = minimumBST l

maximunBST :: BinTree a -> a
maximunBST (NodeB l a EmptyB) = a
maximunBST (NodeB l a r) = maximunBST r

isEmptyB EmptyB = True
isEmptyB _ = False

checkBST :: Ord a => BinTree a -> Bool
checkBST EmptyB = True
checkBST (NodeB l a r) = checkBST l && checkBST r && (if not (isEmptyB l) then a >= maximunBST l else True) && (if not (isEmptyB r) then a < minimumBST r else True) 

smallerOrEqualThanBST :: Ord a => BinTree a -> a -> BinTree a
smallerOrEqualThanBST EmptyB a = EmptyB
smallerOrEqualThanBST (NodeB l b r) a | a == b = NodeB l b EmptyB
                               | a < b = smallerOrEqualThanBST l a
                               | a > b = NodeB l b (smallerOrEqualThanBST r a)

greaterThanBST :: Ord a => BinTree a -> a -> BinTree a
greaterThanBST EmptyB a = EmptyB
greaterThanBST (NodeB l b r) a | a >= b = greaterThanBST r a
                               | a < b = NodeB (greaterThanBST l a) b r

splitBST :: Ord a => BinTree a -> a -> (BinTree a, BinTree a)
splitBST t a = (smallerOrEqualThanBST t a, greaterThanBST t a)

insertBST :: Ord a => a -> BinTree a -> BinTree a
insertBST a EmptyB = NodeB EmptyB a EmptyB
insertBST a (NodeB l b r) | a <= b = NodeB (insertBST a l) b r
                       | otherwise = NodeB l b (insertBST a r)

insertListBST [] t = t
insertListBST (x:xs) t = insertBST x (insertListBST xs t)

joinBST :: Ord a => BinTree a -> BinTree a -> BinTree a
joinBST t1 t2 = insertListBST (inorder t1) t2

memberAuxBST a b EmptyB = a == b
memberAuxBST a b (NodeB l c r) | a <= c = memberAuxBST a c l
                               | otherwise = memberAuxBST a b r

memberBST a t@(NodeB l b r) = memberAuxBST a b t

--4
data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r) | a == b = True
                        | a < b = memberRBT a l
                        | a > b = memberRBT a r

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r

insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT x t = makeBlack (ins x t)
    where ins x E = T R E x E
          ins x (T c l y r) | x < y = lbalance c (ins x l) y r
                            | x > y = rbalance c l y (ins x r)
                            | otherwise = T c l y r
          makeBlack E = E
          makeBlack (T _ l x r) = T B l x r

data OneTwoTree a = Emptree | N2 (OneTwoTree a) a (OneTwoTree a) | N3 (OneTwoTree a) a (OneTwoTree a) a (OneTwoTree a) | N4 (OneTwoTree a) a (OneTwoTree a) a (OneTwoTree a) a (OneTwoTree a)

passRBT2OneTwoTree :: Ord a => RBT a -> OneTwoTree a
passRBT2OneTwoTree E = Emptree
passRBT2OneTwoTree (T B l@(T B _ _ _) a r@(T B _ _ _)) = N2 (passRBT2OneTwoTree l) a (passRBT2OneTwoTree r)
passRBT2OneTwoTree (T B l@(T R lr b rr) a r@(T B _ _ _)) = N3 (passRBT2OneTwoTree lr) b (passRBT2OneTwoTree rr) a (passRBT2OneTwoTree r)
passRBT2OneTwoTree (T B l@(T B _ _ _) a r@(T R ll b rl)) = N3 (passRBT2OneTwoTree l) a (passRBT2OneTwoTree ll) b (passRBT2OneTwoTree rl)
passRBT2OneTwoTree (T B l@(T R lr c rr) a r@(T R ll b rl)) = N4 (passRBT2OneTwoTree lr) c (passRBT2OneTwoTree rr) a (passRBT2OneTwoTree ll) b (passRBT2OneTwoTree rl)

--6
type Rank = Int
data Heap a = EmptyH | N Rank a (Heap a) (Heap a)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 EmptyH = h1
merge EmptyH h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
             else makeH y a2 (merge h1 b2)

rank :: Heap a -> Rank
rank EmptyH = 0
rank (N r _ _ _) = r

makeH x a b = if rank a > rank b then N (rank b + 1) x a b
                                 else N (rank a + 1) x b a

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (N 1 x EmptyH EmptyH) h

findMin :: Ord a => Heap a -> a
findMin (N _ x a b) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (N _ x a b) = merge a b

fromList :: Ord a => [a] -> Heap a
fromList [] = EmptyH
fromList (x:xs) = merge (N 1 x EmptyH EmptyH) (fromList xs)

data PHeaps a = Empty | Root a [PHeaps a]
    deriving Show

pheap = Root 1 [Root 2 [Root 5 []], Root 3 [Root 6 []], Root 4 [Root 7 []]]
nopheap = Root 1 [Root 2 [Root 5 []], Root 3 [Root 6 []], Root 4 [Root 7 [Root 2 []]]]

andList :: [Bool] -> Bool
andList [] = True
andList [x] = x
andList (x:xs) = x && andList xs

isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty = True
isPHeap (Root a xs) = (if not (isEmptyList xs) then a <= minimum(map (\(Root x _) -> x) xs) else True) && andList(map (isPHeap) xs)
                    where isEmptyList [] = True
                          isEmptyList _ = False

mergePHeap :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
mergePHeap n Empty = n 
mergePHeap Empty n = n 
mergePHeap x@(Root a xs) y@(Root b ys) = if a <= b then Root a (y:xs) else Root b (x:ys)

insertPH :: Ord a => PHeaps a -> a -> PHeaps a
insertPH h a = mergePHeap (Root a []) h

concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps [h] = h
concatHeaps (x:xs) = mergePHeap x (concatHeaps xs)

delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty = Nothing
delMin (Root a []) = Just (a, Empty)
delMin (Root a xs) = Just (a, concatHeaps xs)
