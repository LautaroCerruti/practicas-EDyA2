
import Data.Char

-- Ejercicio 1
test :: (Eq a, Num a) => (a -> a) -> a -> Bool
test f x = f x == x + 2

esMenor :: Ord a => a -> a -> Bool
esMenor y z = y < z

eq :: Eq a => a -> a -> Bool
eq a b = a == b

showVal :: Show a => a -> String
showVal x = "Valor:" ++ show x

-- Ejercicio 2
ej2a :: Num a => a -> a
ej2a = (+5) -- suma 5

ej2b :: (Ord a, Num a) => a -> Bool
ej2b = (0<) -- chequea si es mayor a 0

ej2c :: String -> String
ej2c = ('a':) -- Agrega una 'a' al principio de una palabra

ej2d :: String -> String
ej2d = (++"\n") -- Agrega un salto de linea al final

ej2e :: (Eq a, Num a) => [a] -> [a]
ej2e = filter (==7) -- filtra en una lista los valor iguales a 7

ej2f :: Num a => [[a]] -> [[a]]
ej2f = map (++[1])

zip31 :: [a] -> [b] -> [c] -> [(a, b, c)] 
zip31 x y z = map (\(a, (b, c)) -> (a, b, c)) (zip x (zip y z))

zip32 _ [] _ = []
zip32 _ _ [] = []
zip32 [] _ _ = []
zip32 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip32 xs ys zs

modulus = sqrt . sum . map (^2)

vmod [] = []
vmod (v:vs) = modulus v : vmod vs


--ej 12

type NumBin = [Bool]

xor x y = x /= y

sumBinAux [] [] r = if r then [r] else []
sumBinAux (x:xs) [] r = (xor x r) : sumBinAux [] xs (x && r)
sumBinAux [] (y:ys) r = (xor y r) : sumBinAux [] ys (y && r)
sumBinAux (x:xs) (y:ys) r = (xor r (xor y x)) : (sumBinAux xs ys (if x then y || r else y && r))

sumBin :: NumBin -> NumBin -> NumBin
sumBin xs ys = sumBinAux xs ys False

mulBinAux _ [] _ = []
mulBinAux xs (y:ys) s = if y then sumBin (s++xs) (mulBinAux xs ys (False : s)) else  (mulBinAux xs ys (False : s))

mulBin :: NumBin -> NumBin -> NumBin
mulBin xs ys = mulBinAux xs ys []

mulBin2 _ [] = []
mulBin2 xs (y:ys) = if y then sumBin xs (mulBin2 (False : xs) ys) else mulBin2 (False : xs) ys

--deleteLast [x] = []
--deleteLast (x:xs) = x : deleteLast xs

--lastE [x] = x
--lastE (x:xs) = lastE xs 

div2Bin (x:xs) = (xs, x)

--13

divisors x = if x > 0 then [n | n <- [1..x], mod x n == 0] else []

matches x l = [e | e <- l, e == x]

cuadruplas n = [(a, b, c, d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], (a^2+b^2)==(c^2+d^2)]

--unique xs = [e | e <- xs, e' <- xs, e /= e']

unique xs = [ x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

-- 14

scalarP xs ys = sum [ x*y | (x, y) <- zip xs ys]