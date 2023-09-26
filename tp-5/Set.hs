module Set (
    Set,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList
) where

data Set a = S [a] deriving Show

-- Invariantes de Representacion
-- Sea S xs un conjunto
-- 1. xs no tiene elementos repetidos

-- Crea un conjunto vacío

--O(1) constante ya que solo se hace un PM
emptyS :: Set a
emptyS = S []

--Dados un elemento y un conjunto, agrega el 
--elemento al conjunto.

-- O(n) lineal va a recorrer la lista, depende del largo n de la lista

addS :: Eq a => a -> Set a -> Set a
addS x (S xs) =
    if elem x xs
        then S xs
        else S (x : xs)

--Dados un elemento y un conjunto indica si el 
--elemento p ertenece al conjunto.

-- O(n) lineal 

belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = elem x xs


-- O(n)

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs


-- Borra un elemento del conjunto.

-- O(n)

removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs) = S (sinEste x xs)

sinEste :: Eq a => a -> [a] -> [a]
sinEste _ [] = []
sinEste x (y : ys) =
    if x == y
        then ys
        else y : sinEste x ys


-- O(n)

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (unirListas xs ys)

-- O(n)

unirListas :: Eq a => [a] -> [a] -> [a]
unirListas [] ys = ys
unirListas (x:xs) ys =
	if elem x ys
	   then unirListas xs ys
	   else x : unirListas xs ys

-- O(1)

setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs