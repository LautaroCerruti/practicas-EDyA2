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

--5
mapreduce mf rf b s | lengthS s == 0 = b 
                    | lengthS s == 1 = mf (nthS s 0)
                    | otherwise = let NODE l r = showtS s
                                      (le, ri) = mapreduce mf rf b l ||| mapreduce mf rf b r
                                    in rf le ri

cuaturpa v = let au = max v 0
             in (au, au, au, v)

reduche (m, p, s, t) (m', p', s', t') = (maximum [m, m', (s + p')], max p (t + p'), max s' (t' + s), t + t')

mcss t = let (a, b, c, d) = (mapreduce cuaturpa reduche (0, 0, 0, 0) t)
         in a

sccml s = mcss (tabulateS (\i -> if i == (lengthS s - 1) || (nthS s (i+1)) < (nthS s i) then -(lengthS s) else 1) (lengthS s) :: A.Arr Int) + 1

tot = fromList [3, 1, 2, 3, 1, 9] :: A.Arr Int
scctest = fromList [9, 3, 5, 1, 3, 4, 5, 6, 8, 1] :: A.Arr Int
scctest2 = fromList [5, 6, 2, 3, 5, 1, 9] :: A.Arr Int
scctest3 = fromList [1, 4, 6, 7, 8, 11, 12, 3] :: A.Arr Int
{-
sccml2 s = let (i, (t1,t2)) = scanS (\(a,b) (x,y) -> (x, if a < x then b + y else y)) (0,0) (mapS (\x -> (x, 1)) s) 
            in max (mapreduce (\(x,y) -> y) max 0 i) t2 
            -}
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

collect2 :: Ord a => A.Arr (a,b) -> A.Arr (a, A.Arr b)
collect2 s | lengthS s == 0 = emptyS
           | lengthS s == 1 = let (a,b) = nthS s 0
                              in singletonS (a, singletonS b)
           | otherwise = let ordered = sort (\(x, y) (a, b) -> if x > a then GT else if x < a then LT else EQ) s
                         in reduceS f emptyS (mapS (\(x,y) -> singletonS (x, singletonS y)) ordered)
                            where
                                cmp (a, _) (x, _) = if a == x then EQ else if a > x then GT else LT
                                f l r | lengthS l == 0 = r
                                      | lengthS r == 0 = l
                                      | otherwise = case cmp (nthS l (lengthS l-1)) (nthS r 0) of
                                                        EQ ->  appendS (takeS l (lengthS l -1)) (appendS (singletonS (fst (nthS l (lengthS l-1)), appendS (snd (nthS l (lengthS l-1))) (snd (nthS r 0)))) (dropS r 1))
                                                        _ -> appendS l r

l1 = fromList [1,1,1,1,3,3,4,5,1,2,3,3,3,3,11,12,12,12,12] :: A.Arr Int
l2 = fromList [1,2,8,9,10] :: A.Arr Int

l3 = fromList [(2, "A"),(1, "B"),(1, "C"),(2, "D")] :: A.Arr (Int, [Char])

--8

--mapCollectReduce :: (a->(b,c))->((b,c)->(b,c)->(b,c))->(b,c)->A.Arr a -> (b,c)
mapCollectReduce f g s = mapS g (collect2 (mapS f s))

estud = fromList[("Julieta", fromList[40,60,70]), ("Lautaro", fromList[67,87,85]), ("Ramon", fromList[10,40,30]), ("Mimi", fromList[90,95,97])] :: A.Arr ([Char], A.Arr Int)
-- <(2, 70), (1, 87), (3, 40), (1, 97)> -> <(1, <87, 97>), (2, <70>), (3, <40>)>

datosIngreso s = mapCollectReduce mapopo redux s
                    where
                        mapopo (x, sn) = let average = div (reduceS (+) 0 sn) (lengthS sn) 
                                         in if average >= 70 then (1, average) else if average > 50 then (2, average) else (3, average)
                        redux (k, avgs) = (lengthS avgs, reduceS (max) 0 avgs)

--9
countCaract :: A.Arr (A.Arr Char) -> A.Arr (Char, Int)
countCaract s = mapCollectReduce (\x -> (x, 1)) (\(k, xs) -> (k, lengthS xs)) (joinS s)

huffman s = collect2 (mapS (\(x, y) -> (y, x)) (countCaract s))

texts = fromList [fromList "estoy con los flows de ninguno" , fromList "otra noche haciendo plata para bruno", fromList "full ice sera su futuro", fromList "le voy a dar lo que mi madre no tuvo"] :: A.Arr (A.Arr Char)