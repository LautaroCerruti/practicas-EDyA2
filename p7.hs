import Par
import Seq
import ListSeq

promedios a = let (i, t) = scanS (+) 0 a
                  j = appendS (dropS i 1) (singletonS t)
              in tabulateS (\x -> fromIntegral (nthS j x)/ fromIntegral (x+1)) (lengthS j)

mayores :: [Int] -> Int
mayores a = lengthS (
                filterS (emptyS ==) (
                    tabulateS (\x -> filterS ((nthS a x) <=) (takeS a x)) (lengthS a)
                ) :: [[Int]]
            )

test1 = fromList [2, 4, 3, 7, 9] :: [Int]
test2 = fromList [1,2,5,3,5,2,7,9] :: [Int]

cb = (1, 1, 1, 0) --(F_n+1, F_n, F_n, F_n-1)

multM (a, b, c, d) (e, f, g, h) = (a*e+b*f, a*g+b*h, c*e+f*d, c*g+h*d)

fibSeq n = let (i, (a,b,_,_)) = scanS multM cb (tabulateS (\x -> cb) (n-2))
            in appendS (mapS (\(_, v, _, _) -> v) i) (appendS (singletonS b) (singletonS a))

-- 1 1 2 3 5 8 13 21

aguaHist s = let n = lengthS s
                 (maxLs, maxRs) = fst (scanS max 0 s) ||| fst (scanS max 0 (tabulateS (\x -> nthS s (n-x-1)) n :: [Int]))
            in reduceS (+) 0 (tabulateS (\x -> max 0 ((min (nthS maxLs x) (nthS maxRs (n-x-1))) - (nthS s x))) n :: [Int])

hist = fromList [2,3,4,7,5,2,3,2,6,4,3,5,2,1] :: [Int]

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
matchParen2 s = let (i, f) = scanS (+) 0 (mapS parenToInt s :: [Int])
                in emptyS == filterS (0>) i && f == 0 

parente = [Open,Open,Close,Open,Close,Close]
parente2 = [Close,Open]
