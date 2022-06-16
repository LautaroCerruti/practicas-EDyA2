import Data.Char (isDigit, digitToInt)
--1

type Color = (Int, Int, Int)

mezclar :: Color -> Color -> Color
mezclar (x, y, z) (a, b, c) = (div (x+a) 2, div (y+b) 2, div (z+c) 2)

-- 2

type Linea = (String, Int)

vacia :: Linea
vacia = ([], 0)

moverIzq :: Linea -> Linea
moverIzq (xs, 0) = (xs, 0)
moverIzq (xs, i) = (xs, i-1)

moverDer :: Linea -> Linea
moverDer (xs, i) | length xs == i = (xs, i)
                 | otherwise = (xs, i+1)

moverIni :: Linea -> Linea
moverIni (xs, _) = (xs, 0)

moverFin :: Linea -> Linea
moverFin (xs, _) = (xs, length xs)

insertar :: Char -> Linea -> Linea
insertar c (xs, i) = (take i xs ++ [c] ++ drop i xs, i+1)

borrar :: Linea -> Linea
borrar (xs, 0) = (xs, 0)
borrar (xs, i) = (take (i-1) xs ++ drop i xs, i-1)

--3

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnitCL :: CList a -> Bool
isCUnitCL (CUnit _) = True
isCUnitCL _ = False

headCL :: CList a -> a
headCL (CUnit a) = a
headCL (Consnoc a _ _) = a

tailCL :: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc a EmptyCL b) = CUnit b
tailCL (Consnoc a (CUnit c) b) = Consnoc c (EmptyCL) b
tailCL (Consnoc a l b) = Consnoc (headCL l) (tailCL l)  b

reverseCL :: CList a -> CList a
reverseCL (Consnoc a l b) = Consnoc b (reverseCL l) a
reverseCL x = x

--4

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp
    deriving Show

eval:: Exp -> Int 
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = div (eval x) (eval y)

number = (\(Just i)->i)

isNothing Nothing = True
isNothing _ = False

seval (Lit x) = Just x
seval (Add x y) = let 
                    x' = seval x
                    y' = seval y
                  in 
                    if (isNothing x') || (isNothing y') then Nothing else Just ((number x') + (number y'))
seval (Sub x y) = let 
                    x' = seval x
                    y' = seval y
                  in 
                    if (isNothing x') || (isNothing y') then Nothing else Just ((number x') - (number y'))
seval (Prod x y) = let 
                    x' = seval x
                    y' = seval y
                  in 
                    if (isNothing x') || (isNothing y') then Nothing else Just ((number x') * (number y'))
seval (Div x y) = let 
                    x' = seval x
                    y' = seval y
                  in 
                    if (isNothing x') || (isNothing y') || number x' == 0 || number y' == 0 then Nothing else Just ((number x') - (number y'))
--5

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _ = False

operator :: Char -> Exp -> Exp -> Exp
operator '+' = (Add)
operator '-' = (Sub)
operator '*' = (Prod)
operator '/' = (Div)
operator _ = error "not an operator"

parseRPNaux :: String -> [Exp] -> Exp
parseRPNaux [] [e] = e
parseRPNaux (x:xs) l | isDigit x = parseRPNaux xs ((Lit (digitToInt x)) : l)
                     | isOperator x = let (y:z:es) = l
                        in parseRPNaux xs (((operator x) y z) : es)
                     | x == ' ' = parseRPNaux xs l

parseRPN :: String -> Exp
parseRPN l = parseRPNaux l []

evalRPN e = eval (parseRPN e)