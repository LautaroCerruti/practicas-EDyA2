module ArrSeq where

import Arr ((!))
import qualified Arr as A
import Par
import Seq

contraerSA :: (a -> a -> a) -> A.Arr a -> A.Arr a
contraerSA f xs | even l = A.tabulate contraerSP half
                | otherwise = A.tabulate contraerSI (half+1)
                where
                    l = A.length xs
                    half = div l 2
                    contraerSP i = f (xs!(2*i)) (xs!((2*i) + 1))
                    contraerSI i | i == half = xs!(2*half)
                                 | otherwise = contraerSP i

expandirSA :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandirSA f xs is = A.tabulate (\i -> if even i then (is!(div i 2)) else f (is!(div i 2)) (xs!(i-1))) (A.length xs)

instance Seq A.Arr where
    emptyS = A.empty

    singletonS a = fromList [a]

    lengthS = A.length

    nthS = (!)

    tabulateS = A.tabulate

    mapS f xs = tabulateS (\i -> f (nthS xs i)) (lengthS xs)

    filterS f xs = joinS (mapS (\x -> if f x then singletonS x else emptyS) xs)

    appendS xs ys = joinS (fromList [xs,ys])

    takeS xs c = if c >= lengthS xs then xs else A.subArray 0 c xs

    dropS xs c | c == 0 = xs
               | c >= l = emptyS 
               | otherwise = A.subArray (c) (l-c) xs
               where l = lengthS xs

    showtS xs | l == 0 = EMPTY
              | l == 1 = ELT (nthS xs 0)
              | otherwise = let mitad = div l 2
                            in NODE (takeS xs mitad) (dropS xs mitad)
              where l = lengthS xs

    showlS xs | l == 0 = NIL
              | otherwise = CONS (nthS xs 0) (dropS xs 1)
              where l = lengthS xs

    joinS = A.flatten

    reduceS f a xs | l == 0 = a
                   | l == 1 = f a (nthS xs 0)
                   | otherwise = reduceS f a (contraerSA f xs)
                   where
                     l = lengthS xs
 
    scanS f a xs | l == 0 = (emptyS, a)
                 | l == 1 = (singletonS a, f a (nthS xs 0))
                 | otherwise = let (is, r) = scanS f a (contraerSA f xs)
                               in (expandirSA f xs is, r)
                 where 
                    l = lengthS xs

    fromList = A.fromList