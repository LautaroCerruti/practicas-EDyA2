
module Lab02 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List

-- 1) Dada la siguiente definición para representar árboles binarios:

data BTree a = E | Leaf a | Node (BTree a) (BTree a)

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = 0
altura (Leaf _) = 1
altura (Node l r) = 1 + max (altura l) (altura r)

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos
-- y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E = True
perfecto (Leaf _) = True
perfecto (Node l r) = (altura l) == (altura r) && perfecto l && perfecto r

-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf a) = [a]
inorder (Node l r) = (inorder l) ++ (inorder r) 


-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):

data GTree a = EG | NodeG a [GTree a]

data BinTree a = EB | NodeB (BinTree a) a (BinTree a)
    deriving Show

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).
   
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

g2btaux (NodeG a []) [] =  NodeB EB a EB
g2btaux (NodeG a []) (y:ys) =  NodeB EB a (g2btaux y ys)
g2btaux (NodeG a (x:xs)) [] =  NodeB (g2btaux x xs) a EB
g2btaux (NodeG a (x:xs)) (y:ys) = NodeB (g2btaux x xs) a (g2btaux y ys)

g2bt :: GTree a -> BinTree a
g2bt EG = EB
g2bt (NodeG a []) = NodeB EB a EB
g2bt (NodeG a (x:xs)) = NodeB (g2btaux x xs) a EB

concat xs = [(i, x)]

-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
   -}

alturaB EB = 0
alturaB (NodeB l _ r) = 1 + max (alturaB l) (alturaB r) 

dcn :: BinTree a -> [a]
dcn = undefined

lastNs EB = []
lastNs (NodeB EB a EB) = [a]
lastNs (NodeB l a r) = (lastNs l) ++ (lastNs r) 

dcn2 t = lastNs (podar t)
{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2   -}

maxn :: BinTree a -> Int
maxn = undefined

maxn2 t = alturaB (podar t)
{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

isE EB = True
isE _ = False

podar :: BinTree a -> BinTree a
podar EB = EB
podar (NodeB l a r) = let
                        pl = podar l
                        pr = podar r
                    in if (((isE pl) && (isE pr)) || ((not (isE pr)) && (not (isE pl)))) then NodeB pl a pr else NodeB EB a EB

