module Queue (
    Queue,
    emptyQ,
    isEmptyQ,
    enqueue,
    firstQ,
    dequeue
) where

data Queue a = Q [a] deriving Show

--INV.REP
--No tiene

--Crea una cola vacía
--O(1)
emptyQ :: Queue a
emptyQ = Q []

--Dada una cola indica si la cola está vacía
--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _ = False

--Dados un elemento y una cola, agrega ese 
-- elemento a la cola

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (x : xs)

-- Dada una cola devuelve
-- el primer elemento de la cola.

firstQ :: Queue a -> a
firstQ (Q xs) = firstQDeLista xs

firstQDeLista :: [a] -> a
firstQDeLista [x] = x
firstQDeLista (x : xs) = firstQDeLista xs

--Dada una cola la devuelve sin su primer elemento

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (dequeueDeLista xs)

dequeueDeLista :: [a] -> [a]
dequeueDeLista [x] = []
dequeueDeLista (x : xs) = x : dequeueDeLista xs