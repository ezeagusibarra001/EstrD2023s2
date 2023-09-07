data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

mozzarella = Capa Queso Prepizza
jamonYQueso = Capa Jamon mozzarella
dobleMozzarella = Capa Queso mozzarella
cuatroQuesos = Capa Queso (Capa Queso (Capa Queso (Capa Queso Prepizza)))
especial = Capa Jamon (Capa (Aceitunas 8) (Capa Queso (Capa Salsa Prepizza))) 

listaPizzas = [mozzarella, jamonYQueso, dobleMozzarella, cuatroQuesos, especial]

{-Dada una pizza devuelve la cantidad de 
ingredientes-}

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

{-Dada una lista de ingredientes construye una
pizza-}

ingredientes = [Aceitunas 8, Jamon, Queso, Salsa ]

armarPizza :: [Ingrediente] -> Pizza
armarPizza []       = Prepizza
armarPizza (i : is) = (Capa i (armarPizza is))

{-Le saca los ingredientes que sean jamón a la
 pizza-}

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) =
    if esJamon i
        then sacarJamon p
        else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

{- Dice si una pizza tiene solamente salsa y 
queso (o sea, no tiene de otros ingredientes. 
En particular, la prepizza, al no tener ningún
ingrediente, debería dar verdadero.)-}

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) =
    esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

{- Recorre cada ingrediente y si es aceitunas 
duplica su cantidad-}

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) =
    Capa (duplicarAceitunasSi i) (duplicarAceitunas p) 

duplicarAceitunasSi :: Ingrediente -> Ingrediente
duplicarAceitunasSi (Aceitunas n) = Aceitunas (n * 2)
duplicarAceitunasSi i = i

{- Dada una lista de pizzas devuelve un par 
donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza
como segunda componente.-}

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p : ps) =
    (cantidadDeCapas p, p) : cantCapasPorPizza ps

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa1 = Bifurcacion (Cofre [Tesoro]) 
            (Bifurcacion (Cofre [Chatarra]) 
                (Bifurcacion (Cofre [Chatarra]) 
                    (Bifurcacion (Cofre [Chatarra]) 
                        (Fin (Cofre [Tesoro])) 
                        (Fin (Cofre [Chatarra]))) 
                    (Fin (Cofre [Chatarra]))) 
                (Fin (Cofre [Tesoro]))) 
            (Fin (Cofre [Chatarra])) 

{-Indica si hay un tesoro en alguna parte del 
mapa.-}

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) =
    hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = hayTesoroEnLista os

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (o : os) =
    esTesoro o || hayTesoroEnLista os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

{-Indica si al fnal del camino hay un tesoro. 
Nota: el final de un camino se representa con una
lista vacía de direcciones.-}

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m = hayTesoroAca m
hayTesoroEn _ (Fin c) = False
hayTesoroEn (d : ds) (Bifurcacion _ m1 m2) =
    hayTesoroEn ds (mapaASeguir d m1 m2) 

mapaASeguir :: Dir -> Mapa -> Mapa -> Mapa
mapaASeguir Izq m1 _ = m1
mapaASeguir Der _ m2 = m2

hayTesoroAca :: Mapa -> Bool
hayTesoroAca (Fin c) = hayTesoroEnCofre c
hayTesoroAca (Bifurcacion c _ _) = hayTesoroEnCofre c

{- Indica el camino al tesoro. 
Precondición: existe un tesoro y es único.-}

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) =
    if hayTesoroEnCofre c
        then []
        else dirConTesoro m1 m2 : caminoAlTesoro (mapaConTesoro m1 m2)

dirConTesoro :: Mapa -> Mapa -> Dir
dirConTesoro m1 m2 = 
    if hayTesoro m1
        then Izq
        else Der

mapaConTesoro :: Mapa -> Mapa -> Mapa
mapaConTesoro m1 m2 = 
    if hayTesoro m1
        then m1
        else m2

{-Indica el camino de la rama más larga-}

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) =
    if heightM m1 > heightM m2
        then Izq : caminoDeLaRamaMasLarga m1
        else Der : caminoDeLaRamaMasLarga m1

heightM :: Mapa -> Int
heightM (Fin _) = 0
heightM (Bifurcacion _ m1 m2) =
    1 + max (heightM m1) (heightM m2)


{- Devuelve los tesoros separados por 
nivel en el árbol.-}

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [listaDeTesoros c]
tesorosPorNivel (Bifurcacion c m1 m2) =
    listaDeTesoros c : unirPerLevel (tesorosPorNivel m1) (tesorosPorNivel m2) 

unirPerLevel :: [[a]] -> [[a]] -> [[a]]
unirPerLevel [] ys         = ys
unirPerLevel xs []         = xs
unirPerLevel (x:xs) (y:ys) = (x ++ y) : unirPerLevel xs ys


listaDeTesoros :: Cofre -> [Objeto]
listaDeTesoros (Cofre os) = listaDeTesorosObjetos os

listaDeTesorosObjetos :: [Objeto] -> [Objeto]
listaDeTesorosObjetos [] = []
listaDeTesorosObjetos (o : os) = 
    if esTesoro o
        then o : listaDeTesorosObjetos os
        else listaDeTesorosObjetos os

{-Devuelve to dos lo caminos en el mapa-}

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ m1 m2) =
    (Izq : todosLosCaminos m1) ++
    (Der : todosLosCaminos m1)


{- todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT a t1 t2) =
    [a] : 
    agregarATodos a (todosLosCaminos t1)
    ++ agregarATodos a (todosLosCaminos t2)

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x [] = []
agregarATodos x (xs:xss) = (x : xs) : (agregarATodos x xss) -}