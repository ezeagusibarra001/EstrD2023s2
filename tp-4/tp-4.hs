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

mapa1 = Bifurcacion (Cofre [Chatarra]) 
            (Bifurcacion (Cofre [Chatarra]) 
                (Fin (Cofre [Chatarra])) 
                (Fin (Cofre [Chatarra]))) 
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

