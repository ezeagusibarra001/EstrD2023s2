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
poner c celda = (Bolita c celda)

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
alMenosNTesoros n c = n <= (cantidadDeTesoros c)

cantidadDeTesoros :: Camino -> Int
cantidadDeTesoros Fin = 0
cantidadDeTesoros (Nada c) = cantidadDeTesoros c
cantidadDeTesoros (Cofre os c) =  
    cantidadDeTesorosAca os + cantidadDeTesoros c

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

{-2.1.2 Dado un árbol binario devuelve su
cantidad de elementos, es decir, el tamaño 
del árbol (size en inglés) -}

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ tl tr) =
    1 + sizeT tl + sizeT tr

{-2.1.3 Dado un árb ol de enteros devuelve un
árbol con el doble de cada número.-}

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n tl tr) =
    (NodeT (n * 2) (mapDobleT tl) (mapDobleT tr)) 

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
heightT EmptyT
heightT (NodeT e tr tl) = 