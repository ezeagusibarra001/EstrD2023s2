import Stack

pila = push 6 (push 2 emptyS)

--Dada una lista devuelve una pila 
--sin alterar el orden de los elementos

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x : xs) = push x (apilar xs)


--Dada una pila devuelve una lista sin 
--alterar el orden de los elementos

desapilar :: Stack a -> [a]
desapilar stack =
    if isEmptyS stack
        then []
        else (top stack) : desapilar (pop stack)


{-
Dada una posicion válida en la stack y un elemento, 
ubica dicho elemento en dicha
posición (se desapilan elementos hasta dicha 
posición y se inserta en ese lugar)
-}

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 a st = push a st 
insertarEnPos x a st = insertarEnPos (x-1) a (pop st)