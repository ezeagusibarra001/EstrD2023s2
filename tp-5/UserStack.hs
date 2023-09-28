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