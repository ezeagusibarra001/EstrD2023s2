import Set
import Queue

data Tree a = NodeT a (Tree a) (Tree a)
			| EmptyT deriving Show

set = addS 2 (addS 1 emptyS)
set2 = addS 4 (addS 1 emptyS)


{-  Dados una lista y un conjunto, 
    devuelve una lista con to dos los 
    elementos que p ertenecen
    al conjunto.
-}

--O(n)

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x :xs) xset =
    if belongs x xset
        then x : losQuePertenecen xs xset
        else losQuePertenecen xs xset


{-
Quita todos los elementos repetidos de la lista 
dada utilizando un conjunto como estructura auxiliar
-}

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x : xs) =
    if elem x xs
        then sinRepetidos xs
        else x : sinRepetidos xs


{-
Dado un arbol de conjuntos devuelve un 
conjunto con la union de to dos los conjuntos
del arbol
-}

arbol = NodeT set (NodeT set2 EmptyT EmptyT) EmptyT

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2) =
    unionS s (unionS (unirTodos t1) (unirTodos t2))

-----------------------------------------------------
queue = enqueue 3 (enqueue 2 emptyQ)

--Cuenta la cantidad de elementos de la cola.

lengthQ :: Queue a -> Int
lengthQ qe =
    if isEmptyQ qe
        then 0
        else 1 + lengthQ (dequeue qe)

{-
Dada una cola devuelve la lista con los mismos 
elementos,donde el orden de la lista es el 
de la cola
-}

queueToList :: Queue a -> [a]
queueToList qe =
    if isEmptyQ qe
        then []
        else firstQ qe : queueToList (dequeue qe)

--Inserta to dos los elementos de
-- la segunda cola en la primera.

unionQ :: Queue a -> Queue a -> Queue a
unionQ qe1 qe2 =
    if isEmptyQ qe2
        then qe1
        else unionQ (enqueue (firstQ qe2) qe1) (dequeue qe2) 
