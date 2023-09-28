module Stack(
    Stack,
    emptyS,
    isEmptyS,
    push,
    top,
    pop,
    lenS
) where

data Stack a = S [a] Int deriving Show

{-
    Dado un stack (S xs n)
    INV.REP
    1) length xs === n
-}


--Crea una pila vacía

emptyS :: Stack a
emptyS = S [] 0

--Dada una pila indica si está vacía

isEmptyS :: Stack a -> Bool
isEmptyS (S xs _) = null xs

--Dados un elemento y una pila,
-- agrega el elemento a la pila.

push :: a -> Stack a -> Stack a
push x (S xs n) = S (x : xs) (n + 1)


--Dada un pila devuelve 
--el elemento del top e de la pila

top :: Stack a -> a
top (S xs _) = head xs


--Dada una pila devuelve la 
--pila sin el primer elemento

pop :: Stack a -> Stack a
pop (S xs n) = S (tail xs) (n - 1)

--Dada la cantidad de elementos en la pila.
--Costo: constante.

lenS :: Stack a -> Int
lenS (S _ n) = n

