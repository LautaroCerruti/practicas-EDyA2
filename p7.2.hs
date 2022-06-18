import Par
import Seq
import ArrSeq
import qualified Arr as A

promedios a = let (i, t) = scanS (+) 0 a
                  j = appendS (dropS i 1) (singletonS t)
              in tabulateS (\x -> fromIntegral (nthS j x)/ fromIntegral (x+1)) (lengthS j)

isEmpty s = if lengthS s == 0 then True else False

mayores :: A.Arr Int -> Int
mayores a = lengthS (
                filterS isEmpty (
                    tabulateS (\x -> filterS ((nthS a x) <=) (takeS a x)) (lengthS a)
                ) :: A.Arr (A.Arr Int)
            )

test1 = fromList [2, 4, 3, 7, 9] :: A.Arr Int
test2 = fromList [1,2,5,3,5,2,7,9] :: A.Arr Int

cb = (1, 1, 1, 0) --(F_n+1, F_n, F_n, F_n-1)

multM (a, b, c, d) (e, f, g, h) = (a*e+b*f, a*g+b*h, c*e+f*d, c*g+h*d)

fibSeq n = let (i, (a,b,_,_)) = scanS multM cb (tabulateS (\x -> cb) (n-2) :: A.Arr (Int,Int,Int,Int))
            in appendS (mapS (\(_, v, _, _) -> v) i) (appendS (singletonS b) (singletonS a))

-- 1 1 2 3 5 8 13 21

aguaHist s = let n = lengthS s
                 (maxLs, maxRs) = fst (scanS max 0 s) ||| fst (scanS max 0 (tabulateS (\x -> nthS s (n-x-1)) n :: A.Arr Int))
            in reduceS (+) 0 (tabulateS (\x -> max 0 ((min (nthS maxLs x) (nthS maxRs (n-x-1))) - (nthS s x))) n :: A.Arr Int)

hist = fromList [2,3,4,7,5,2,3,2,6,4,3,5,2,1] :: A.Arr Int

data Paren = Open | Close
matchParen1 s = matchP s == (0, 0)

matchP s | lengthS s == 1 = case nthS s 0 of
                                Open -> (0,1)
                                Close -> (1,0)
         | otherwise = let NODE a b = showtS s
                           ((l, r), (l', r')) = matchP a ||| matchP b
                       in mergeparent (l, r) (l', r')
                            where
                                mergeparent (l, r) (l', r') = let min1 = min l r
                                                                  min2 = min l' r'
                                                                  (a, b) = (l - min1, r - min1)
                                                                  (a', b') = (l' - min2, r' - min2)
                                                                  min3 = min a' b
                                                            in (a - min3 + a', b + b' - min3)

parenToInt Open = 1
parenToInt Close = -1
matchParen2 s = let (i, f) = scanS (+) 0 (mapS parenToInt s :: A.Arr Int)
                in isEmpty (filterS (0>) i) && f == 0 

parente = fromList [Open,Open,Close,Open,Close,Close] :: A.Arr Paren
parente2 = fromList [Close,Open] :: A.Arr Paren

--6
multiplos s = lengthS (filterS (0==) (joinS (tabulateS (\i -> mapS (mod (nthS s i)) (dropS s (i+1))) ((lengthS s)-1) :: A.Arr (A.Arr Int))))

mulsdf = fromList [12,4,6,3,2] :: A.Arr Int

--7

lt :: (a -> a -> Ordering) -> a -> a -> Bool
lt f x y = f y x == LT

gteq :: (a -> a -> Ordering) -> a -> a -> Bool
gteq f x y = f y x == GT || f y x == EQ

splitAtT :: (a->a->Ordering) -> A.Arr a -> a -> (A.Arr a, A.Arr a)
splitAtT f s x | lengthS s == 0 = (emptyS, emptyS)
               | otherwise = filterS (lt f x) s ||| filterS (gteq f x) s

mergeS f s s' | lengthS s == 0 = s'
              | lengthS s' == 0 = s
              | otherwise = let (m1, m2) = case showtS s of 
                                                NODE a1 a2 -> (a1, a2) 
                                                ELT a1 -> (emptyS, singletonS a1)
                                m = nthS m2 0
                                m2' = dropS m2 1
                                (s1, s2) = splitAtT f s' m
                                (merge1, merge2) = mergeS f m1 s1 ||| mergeS f m2' s2
                            in appendS merge1 (appendS (singletonS m) merge2)

sort f s = reduceS (mergeS f) emptyS (mapS singletonS s)

maxOrd :: (a -> a -> Ordering) -> a -> a -> a
maxOrd f x y = let r = f x y 
             in if r == GT || r == EQ then x else y 

maxE f s | lengthS s == 1 = nthS s 0
         | lengthS s >= 2 = reduceS (maxOrd f) (nthS s 0) s

maxS f s | lengthS s == 1 = 0
         | lengthS s >= 2 = let (maximo, tab) = maxE f s ||| (tabulateS (\x -> x) (lengthS s) :: A.Arr Int)
                            in nthS (filterS (\x -> (nthS s x) == maximo) tab) 0

groupS :: (a -> a -> Ordering) -> A.Arr a -> A.Arr a
groupS f s | lengthS s <= 2 = s
           | otherwise = joinS (tabulateS (\i -> aux i) n)
           where 
            n = lengthS s
            aux i = if i /= (n-1) then (if (f (nthS s i) (nthS s (i+1))) == EQ then emptyS else singletonS (nthS s i)) else singletonS (nthS s i)

compi x y = if x > y then GT else if x < y then LT else EQ

collect :: Ord a => A.Arr (a, b) -> A.Arr (a, A.Arr b)
collect s | lengthS s == 0 = emptyS
          | lengthS s == 1 = let (a,b) = nthS s 0
                             in singletonS (a, singletonS b)
          | otherwise = let 
                            keys = groupS (\a b -> if a > b then GT else if a < b then LT else EQ) (sort compi (mapS (\(x, y) -> x) s))
                        in mapS (\x -> (x, mapS (\(x, y) -> y) (filterS (\(y, z) -> y == x) s))) keys

l1 = fromList [1,1,1,1,3,3,4,5,1,2,3,3,3,3,11,12,12,12,12] :: A.Arr Int
l2 = fromList [1,2,8,9,10] :: A.Arr Int

l3 = fromList [(2, "A"),(1, "B"),(1, "C"),(2, "D")] :: A.Arr (Int, [Char])