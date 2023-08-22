--1---------------------------

--1.1 Dada una lista de enteros devuelve la suma de to dos sus elementos.

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + (sumatoria ns)

{-1.2 Dada una lista de elementos de algún 
tipo devuelve el largo de esa lista, es decir, 
la cantidad de elementos que p osee. -}

longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

{-1.3 Dada una lista de enteros, devuelve la lista
de los sucesores de cada entero.-}

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n : ns) = (n + 1) : sucesores ns

{-1.4 Dada una lista de booleanos devuelve True si
 todos sus elementos son True.-}

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b : bs) = b && conjuncion bs 

{-1.5 Dada una lista de booleanos devuelve True 
si alguno de sus elementos es True.-}

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b : bs) = b || disyuncion bs 

{-1.6 Dada una lista de listas, devuelve una
 única lista con to dos sus elementos.-}

lista1 = [1,2,3,4,5]
lista2 = [1,2,3,4,5,6,7,8,9,10]
lista3 = [1,2,3,4,5,2,3,4]

listaDeListas = [lista1, lista2, lista3]

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs : xss) = xs ++ aplanar xss

{-1.7 Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
a e.-}

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x : xs) = e == x || pertenece e xs

{-1.8 Dados un elemento e y una lista xs cuenta 
la cantidad de apariciones de e en xs.-}

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x : xs) = unoSiCeroSino (e == x) + apariciones e xs

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

{-1.9  Dados un número n y una lista xs, devuelve 
todos los elementos de xs que son menores a n.-}

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x : xs) = if x < n
    then x : losMenoresA n xs
    else losMenoresA n xs

{-1.10 Dados un número n y una lista de listas, 
devuelve la lista de aquellas listas que tienen 
más de n elementos.-}

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (xs : xss) = 
    if longitud xs > n
        then xs : lasDeLongitudMayorA n xss
        else lasDeLongitudMayorA n xss

{-1.11 Dados una lista y un elemento, devuelve una
lista con ese elemento agregado al final de 
la lista.-}

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x = [x]
agregarAlFinal (l : ls) x = l : agregarAlFinal ls x

{-1.12 Dadas dos listas devuelve la lista con 
todos los elementos de la primera lista y todos 
los elementos de la segunda a continuación. (++)-}

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x : xs) ys = x : agregar xs ys

{-1.13 Dada una lista devuelve la lista con los 
mismos elementos de atrás para adelante. Definida
en Haskell como reverse.-}

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

{-1.14 Dadas dos listas de enteros, devuelve una 
lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y
de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma
longitud.-}

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x : xs) ys = if x > head ys
        then x : zipMaximos xs (tail ys)
        else (head ys) : zipMaximos xs (tail ys)

{-1.15 Dada una lista devuelve el mínimo-}

elMinimo :: Ord a => [a] -> a
-- PRECOND: La lista no puede estar vacia
elMinimo [] = error "No se puede calcular el minimo de []"
elMinimo (x : []) = x
elMinimo (x : xs) = min x (elMinimo xs)

min' :: Ord a => a -> a -> a
min' x y = if x < y
    then x
    else y