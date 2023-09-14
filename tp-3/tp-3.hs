data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

{-1.1.1 Dados un color y una celda, indica la 
cantidad de bolitas de ese color. 
Nota: pensar si ya existe una operación sobre 
listas que ayude a resolver el problema.-}

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita c2 celda) =
    unoSiCeroSino (esMismoColor c c2) + nroBolitas c celda

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False


{-3.1.2 Dado un color y una celda, agrega una bolita
de dicho color a la celda.-}

poner :: Color -> Celda -> Celda
poner c celda = Bolita c celda

{-3.1.3 Dado un color y una celda, quita una bolita
de dicho color de la celda. 
Nota: a diferencia de Gobstones, esta función es total-}

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita c2 celda) = 
    if esMismoColor c c2
        then celda
        else Bolita c2 (sacar c celda)

{-3.1.4 Dado un número n, un color c, y una celda, 
agrega n bolitas de color c a la celda.-}

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n c celda = Bolita c (ponerN (n-1) c celda)

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

caminoAlTesoro = Nada (Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Cacharro] Fin)))
caminoAlNoTesoro = Nada (Cofre [Cacharro, Cacharro] (Nada (Cofre [Cacharro] Fin)))

caminoConMuchosTesoros = Nada (Cofre [Tesoro, Tesoro,Tesoro] (Nada (Cofre [Tesoro, Tesoro] Fin)))

{-1.2.1 Indica si hay un cofre con un tesoro en el camino.-}

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre os c) = hayTesoroAca os || hayTesoro c

hayTesoroAca :: [Objeto] -> Bool
hayTesoroAca [] = False
hayTesoroAca (o : os) = esTesoro o || hayTesoroAca os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

{-1.2.2 Indica la cantidad de pasos que hay que recorrer hasta
llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, 
la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.-}

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Fin) = error "Tiene que haber al menos un tesoro."
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre os c) = 
    if hayTesoroAca os
        then 0
        else 1 + pasosHastaTesoro c

{-1.2.3 Indica si hay un tesoro en una cierta cantidad exacta 
de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.-}

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn n (Nada c) = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre os c) = 
    (n == 0 && hayTesoroAca os) || hayTesoroEn (n-1) c

{-1.2.4 Indica si hay al menos ntesoros en el camino-}

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre os c) = alMenosNTesoros (losTesorosQueMeQuedan n os) c

losTesorosQueMeQuedan :: Int -> [Objeto] -> Int
losTesorosQueMeQuedan 0 _ = 0
losTesorosQueMeQuedan n [] = n
losTesorosQueMeQuedan n (o : os) = losTesorosQueMeQuedan (n - unoSiCeroSino (esTesoro o)) os

cantidadDeTesorosAca :: [Objeto] -> Int
cantidadDeTesorosAca [] = 0
cantidadDeTesorosAca (o : os) = 
    unoSiCeroSino (esTesoro o) + cantidadDeTesorosAca os


{-1.2.5 Dado un rango de pasos, indica la cantidad de tesoros 
que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre
hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.-}

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ _ Fin = 0
cantTesorosEntre 0 m camino = contarTesorosHasta m camino 
cantTesorosEntre n m (Nada c) = cantTesorosEntre (n-1) (m-1) c
cantTesorosEntre n m (Cofre obs c) = cantTesorosEntre (n-1) (m-1) c

contarTesorosHasta :: Int -> Camino -> Int
contarTesorosHasta n Fin = 0
contarTesorosHasta 0 c   = 0
contarTesorosHasta n (Nada c) = contarTesorosHasta (n-1) c
contarTesorosHasta n (Cofre obs c) = cantidadDeTesorosAca obs + contarTesorosHasta (n-1) c

-----------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbol :: Tree Int
arbol = NodeT 10 
            (NodeT 5 
                (NodeT 9 
                    EmptyT 
                    EmptyT) 
                EmptyT) 
            (NodeT 2 
                EmptyT 
                EmptyT) 


{-2.1.1 Dado un árbol binario de enteros 
devuelve la suma entre sus elementos-}

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n tl tr) =
    n + sumarT tl + sumarT tr

{-Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño 
del árb ol (size en inglés)-}
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ tl tr) =
    1 + sizeT tl + sizeT tr

{-2.1.3 Dado un árb ol de enteros devuelve un
árbol con el doble de cada número.-}

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n tl tr) =
    NodeT (n * 2) (mapDobleT tl) (mapDobleT tr)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e (NodeT e1 tl tr) =
    e == e1 || (perteneceT e tl) || (perteneceT e tr)

{-2.1.4 Dados un elemento e y un árbol binario devuelve
la cantidad de elementos del árbol que son iguales a e.-}

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT e1 tl tr) =
    unoSiCeroSino(e == e1) + (aparicionesT e tl) + (aparicionesT e tr)

{-2.1.5 Dado un árb ol devuelve los elementos
que se encuentran en sus hojas-}

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT e EmptyT EmptyT) = [e]
leaves (NodeT _ tl tr) = (leaves tl) ++ (leaves tr)

{-2.1.7 Dado un árb ol devuelve su altura-}

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ tl tr) =
    1 + max (heightT tl) (heightT tr)

{-2.1.8 Dado un árbol devuelve el árbol resultante
de intercambiar el hijo izquierdo con el derecho,
en cada nodo del árbol-}

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT e tl tr) =
    NodeT e (mirrorT tr) (mirrorT tl)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT e tl tr) =
    toList tl ++ [e] ++ toList tr

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT e _ _) = [e]
levelN n (NodeT e tl tr) =
    levelN (n-1) tl ++ levelN (n-1) tr

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT a t1 t2) = [a] : unirPerLevel (listPerLevel t1) (listPerLevel t2)

unirPerLevel :: [[a]] -> [[a]] -> [[a]]
unirPerLevel [] ys         = ys
unirPerLevel xs []         = xs
unirPerLevel (x:xs) (y:ys) = (x ++ y) : unirPerLevel xs ys

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT a t1 t2) = a : masLargoEntre (ramaMasLarga t1) (ramaMasLarga t2)

masLargoEntre :: [a] -> [a] -> [a]
masLargoEntre [] ys = ys
masLargoEntre xs [] = xs
masLargoEntre xs ys    = 
    if length xs > length ys
        then xs
        else ys

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) =
    [x] : 
    agregarATodos x (todosLosCaminos t1)
    ++ agregarATodos x (todosLosCaminos t2)

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x [] = []
agregarATodos x (xs:xss) = (x : xs) : (agregarATodos x xss)

data ExpA = Valor Int
    | Sum ExpA ExpA
    | Prod ExpA ExpA
    | Neg ExpA deriving Show

-- eval :: ExpA -> -> Int
-- Dada una expresión aritmética devuelve el resultado evaluarla.

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum v1 v2) = eval v1 + eval v2
eval (Prod v1 v2) = eval v1 * eval v2
eval (Neg v) = -(eval v)

-- eval (Sum (Prod (Valor 2) (Valor 3)) (Valor 4)) = 10

-- simplificar :: ExpA -> ExpA
-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):
-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x

simplificar :: ExpA -> ExpA
simplificar (Valor n) = Valor n
simplificar (Sum v1 v2) = simplificarSuma (simplificar v1) (simplificar v2)
simplificar (Prod v1 v2) = simplificarProducto (simplificar v1) (simplificar v2)
simplificar (Neg v) = simplificarNegacion (simplificar v)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) v = v
simplificarSuma v (Valor 0) = v
simplificarSuma v1 v2 = Sum v1 v2

simplificarProducto :: ExpA -> ExpA -> ExpA
simplificarProducto (Valor 0) v = Valor 0
simplificarProducto v (Valor 0) = Valor 0
simplificarProducto (Valor 1) v = v
simplificarProducto v (Valor 1) = v
simplificarProducto v1 v2 = Prod v1 v2


simplificarNegacion :: ExpA -> ExpA
simplificarNegacion (Neg v) = v
simplificarNegacion v = Neg v

-- simplificar (Sum (Valor 0) (Valor 3)) = Valor 3