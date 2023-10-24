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

{-Devuelve todos lo caminos en el mapa-}

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ m1 m2) =
    agregarATodos Izq (todosLosCaminos m1)
    ++ agregarATodos Der (todosLosCaminos m1)

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x [] = [[x]]
agregarATodos x (xs:xss) = (x : xs) : (agregarATodos x xss)


{- todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT a t1 t2) =
    [a] : 
    agregarATodos a (todosLosCaminos t1)
    ++ agregarATodos a (todosLosCaminos t2)
 -}


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show

sPuente   = S "Puente" [Motor 1, LanzaTorpedos] ["Capitan", "Oficial de Navegacion"]
sMaquinas = S "Sala de Maquinas" [Motor 2, Almacen [Combustible, Combustible, Oxigeno]] ["Ingeniero de Maquinas", "Tecnico de Almacenamiento"]
sControl  = S "Sala de Control" [Almacen [Comida, Torpedo], LanzaTorpedos] ["Operador de Control", "Tecnico de Almacenamiento"]

lComponentes = [Motor 99]

nave :: Nave
nave = N
  (NodeT sPuente
    (NodeT sMaquinas
      EmptyT
      EmptyT)
    (NodeT sControl
      EmptyT
      EmptyT)
  )

sectorId :: Sector -> SectorId
sectorId (S id _ _) = id

-- Prop ósito: Devuelve to dos los sectores de la nave.

sectores :: Nave -> [SectorId]
sectores (N arbol) = sectoresDeArbol arbol

sectoresDeArbol :: Tree Sector -> [SectorId]
sectoresDeArbol EmptyT = []
sectoresDeArbol (NodeT s t1 t2) = 
    sectorId s : sectoresDeArbol t1 ++ sectoresDeArbol t2


{- Propósito: Devuelve la suma de poder de 
propulsión de to dos los motores de la nave. 
Nota: el poder de propulsión es el número
que acompaña al constructor de motores.-}

poderDePropulsion :: Nave -> Int
poderDePropulsion (N arbol) = propulsionDeArbol arbol

propulsionDeArbol :: Tree Sector -> Int
propulsionDeArbol EmptyT          = 0
propulsionDeArbol (NodeT s t1 t2) =
    propulsionDeSector s + propulsionDeArbol t1 + propulsionDeArbol t2

propulsionDeSector :: Sector -> Int
propulsionDeSector (S _ cs _) =
    propulsionDeLosMotores cs

propulsionDeLosMotores :: [Componente] -> Int
propulsionDeLosMotores [] = 0
propulsionDeLosMotores (c : cs) =
    propulsionDeMotor c + propulsionDeLosMotores cs

propulsionDeMotor :: Componente -> Int
propulsionDeMotor (Motor n) = n
propulsionDeMotor _ = 0

{-Propósito: Devuelve todos los barriles de la 
nave.-}

barriles :: Nave -> [Barril]
barriles (N arbol) = barrilesDeArbol arbol

barrilesDeArbol :: Tree Sector -> [Barril]
barrilesDeArbol EmptyT = []
barrilesDeArbol (NodeT s t1 t2) =
    barrilesDeSector s ++
        barrilesDeArbol t1 ++ barrilesDeArbol t2

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector (S _ cs _ ) = barrilesDeComponentes cs

barrilesDeComponentes :: [Componente] -> [Barril]
barrilesDeComponentes [] = []
barrilesDeComponentes (c : cs) =
    barrilDeComponente c ++ barrilesDeComponentes cs

barrilDeComponente :: Componente -> [Barril]
barrilDeComponente (Almacen bs) = bs
barrilDeComponente _ = []


{-
Propósito: Añade una lista de componentes a un 
sector de la nave.
Nota: ese sector puede no existir, en cuyo caso no
añade componentes.
-}

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sId (N arbol) =
   N (agregarASectorArbol cs sId arbol)

agregarASectorArbol :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorArbol cs sId (EmptyT) = EmptyT
agregarASectorArbol cs sId (NodeT s t1 t2) = 
    NodeT (agregarASectorSi cs sId s) (agregarASectorArbol cs sId t1) (agregarASectorArbol cs sId t2)

agregarASectorSi :: [Componente] -> SectorId -> Sector -> Sector
agregarASectorSi cs sId s =
    if sId == sectorId s
        then agregarCsASector cs s
        else s

agregarCsASector :: [Componente] -> Sector -> Sector
agregarCsASector cs' (S id cs ts) = S id (cs' ++ cs) ts

{-
Proposito: Incorpora un tripulante a una lista de sectores 
de la nave.
Precondición: Todos los id de la lista existen en la nave.
-}

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ids (N tree) = N (asignarTAArbol t ids tree)

asignarTAArbol :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTAArbol t ids EmptyT = EmptyT
asignarTAArbol t ids (NodeT s t1 t2) =
    NodeT (asignarTSi t ids s)  (asignarTAArbol t ids t1) (asignarTAArbol t ids t2)


asignarTSi :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTSi t ids s =
    if estaEn (sectorId s) ids
        then agregarTripuA t s
        else s

estaEn :: SectorId -> [SectorId] -> Bool
estaEn _ [] = False
estaEn sid (i : ids) =
    if sid == i
        then True
        else estaEn sid ids

agregarTripuA :: Tripulante -> Sector -> Sector
agregarTripuA t (S i cs ts) = S i cs (t : ts)

{-Propósito: Devuelve los sectores en donde aparece un 
tripulante dado.-}

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N tree) = sectoresAsignadosArbol t tree

sectoresAsignadosArbol :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosArbol _ EmptyT = []
sectoresAsignadosArbol t (NodeT s t1 t2) =
    if apareceTEn t (tripulantesDeS s)
        then (sectorId s) : sectoresAsignadosArbol t t1 ++ sectoresAsignadosArbol t t2
        else sectoresAsignadosArbol t t1 ++ sectoresAsignadosArbol t t2

tripulantesDeS :: Sector -> [Tripulante]
tripulantesDeS (S _ _ ts) = ts

apareceTEn :: Tripulante -> [Tripulante] -> Bool
apareceTEn _ [] = False
apareceTEn tr (t : ts) = tr == t || apareceTEn tr ts

{-Propósito: Devuelve la lista de tripulantes, 
sin elementos rep etidos.-}

tripulantes :: Nave -> [Tripulante]
tripulantes (N tree) = tripulantesArbol tree

tripulantesArbol :: Tree Sector -> [Tripulante]
tripulantesArbol EmptyT = []
tripulantesArbol (NodeT s t1 t2) =
   (tripulantesDeS s) ++ (tripulantesArbol t1) ++ (tripulantesArbol t2)


-----------------------------------------------------------------

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
    | Explorador Nombre [Territorio] Lobo Lobo
    | Cria Nombre
    deriving Show

data Manada = M Lobo
    deriving Show


-- Definimos algunas presas y territorios
presas1 = ["Conejo", "Venado"]
territorios1 = ["Bosque", "Montaña"]

presas2 = ["Liebre", "Ciervo"]
territorios2 = ["Pradera", "Bosque"]

-- Creamos algunos lobos
cazador = Cazador "Cazador" presas1 explorador1 explorador2 (Cria "Cria3")
explorador1 = Explorador "Explorador 1" territorios1 (Cria "Cria4") (Cria "Cria5")
explorador2 = Explorador "Explorador 2" territorios2 (Cria "Cria4") (Cria "Cria5")

-- Creamos una manada con estos lobos
manada = M cazador


pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = x == a || pertenece a xs

sinRepetidos ::Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
	if pertenece x xs
		then sinRepetidos xs
		else x : sinRepetidos xs

{-Dada una manada, indica si la cantidad de 
alimento cazado es mayor a la cantidad de crías.-}

buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantDeCrias l <= cantDePresasTotales l

cantDeCrias :: Lobo -> Int
cantDeCrias (Cria n) = 1
cantDeCrias (Explorador n ts l1 l2) = cantDeCrias l1 + cantDeCrias l2
cantDeCrias (Cazador n ts l1 l2 l3) = cantDeCrias l1 + cantDeCrias l2 + cantDeCrias l3

cantDePresasTotales :: Lobo -> Int
cantDePresasTotales (Cria n) = 0
cantDePresasTotales (Explorador n ts l1 l2) = cantDePresasTotales l1 + cantDePresasTotales l2
cantDePresasTotales (Cazador n ps l1 l2 l3) = 
	length ps + cantDePresasTotales l1 + cantDePresasTotales l2 + cantDePresasTotales l3

cantDePresas :: Lobo -> Int
cantDePresas (Cazador _ ps _ _ _) = length ps
cantDePresas _					  = 0

elAlfa :: Manada -> (Nombre, Int)
elAlfa m = (nombreLobo (elMasCazadorManada m), cantDePresas (elMasCazadorManada m))

elMasCazadorManada :: Manada -> Lobo
elMasCazadorManada m = elMasCazador (cazadores m)

elMasCazador :: [Lobo] -> Lobo
elMasCazador [x] = x
elMasCazador (x:xs) = 
	if cantDePresas x > cantDePresas (elMasCazador xs)
		then x
		else elMasCazador xs 

cazadores :: Manada -> [Lobo]
cazadores (M l) = cazadoresL l

cazadoresL :: Lobo -> [Lobo]
cazadoresL (Cria n) = []
cazadoresL (Explorador n ts l1 l2) = cazadoresL l1 ++ cazadoresL l2
cazadoresL (Cazador n ps l1 l2 l3) = 
	(Cazador n ps l1 l2 l3) :
	cazadoresL l1 ++
	cazadoresL l2 ++
	cazadoresL l3

esCazador :: Lobo -> Bool
esCazador (Cria _) = False
esCazador (Explorador _ _ _ _) = False
esCazador (Cazador _ _ _ _ _)  = True

nombreLobo :: Lobo -> Nombre
nombreLobo (Cria n) 		    = n
nombreLobo (Explorador n _ _ _) = n
nombreLobo (Cazador n _ _ _ _)  = n

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cria n) = []
losQueExploraronL t (Explorador n ts l1 l2) = 
	if pertenece t ts
		then n : losQueExploraronL t l1 ++ losQueExploraronL t l2
		else losQueExploraronL t l1 ++ losQueExploraronL t l2
losQueExploraronL t (Cazador n ps l1 l2 l3) =
	losQueExploraronL t l1 ++ 
	losQueExploraronL t l2 ++ 
	losQueExploraronL t l3

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio m = exploradoresPorTerritorioM m (territoriosSinRep m)

exploradoresPorTerritorioM :: Manada -> [Territorio] -> [(Territorio, [Nombre])]
exploradoresPorTerritorioM m [] 		= []
exploradoresPorTerritorioM m (t:ts) = 
	generadorDeTuplas t (losQueExploraron t m) : exploradoresPorTerritorioM m ts 

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superioresDelCazadorL n l

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL n (Cria n1) = []
superioresDelCazadorL n (Explorador n1 ts l1 l2) = 
	superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2
superioresDelCazadorL n (Cazador n1 ps l1 l2 l3) = 
	if n == n1
		then []
		else n1 : superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2 ++ superioresDelCazadorL n l3

generadorDeTuplas :: a -> b -> (a, b)
generadorDeTuplas a b = (a, b)

territoriosSinRep :: Manada -> [Territorio]
territoriosSinRep (M l) = territoriosSinRepL l

territoriosSinRepL :: Lobo -> [Territorio]
territoriosSinRepL (Cria n) = []
territoriosSinRepL (Cazador n ps l1 l2 l3) =
    sinRepetidos (territoriosSinRepL l1 ++
        					territoriosSinRepL l2 ++
        					territoriosSinRepL l3)
territoriosSinRepL (Explorador n ts l1 l2) =
    sinRepetidos (ts ++
        					territoriosSinRepL l1 ++
        					territoriosSinRepL l2)