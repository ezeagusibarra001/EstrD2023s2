import Set

data Tree a = NodeT a (Tree a) (Tree a)
			| EmptyT

set = addS 2 (addS 1 emptyS)

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

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2) =
    unionS s (unionS (unirTodos t1) (unirTodos t2))