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

--2-------------------------------------------

{-2.1 Dado un número n se devuelve la 
multiplicación de este número y todos sus 
anteriores hasta llegar a 0. Si n es 0 
devuelve 1. La función es parcial si n es 
negativo. -}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

{-2.1 Dado un número n devuelve una lista cuyos
elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 
1, devuelve la lista vacía.-}

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = [] -- este caso no es necesario, pero lo pongo para que quede claro que lo tengo en cuenta 
cuentaRegresiva n = 
    if n < 1
        then []
        else n : cuentaRegresiva (n-1)

{-2.2 Dado un número n y un elemento e devuelve
una lista en la que el elemento e repite n
veces.-}

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e 

{-2.3 Dados un número n y una lista xs, 
devuelve una lista con los n primeros elementos
de xs. Si la lista es vacía, devuelve una lista
vacía.-}

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x : xs) = x : losPrimeros (n-1) xs

{-2.3 Dados un número n y una lista xs, 
devuelve una lista sin los primeros n elementos
de lista recibida. Si n es cero, devuelve la 
lista completa.-}

sinLosPrimeros :: Int -> [a] -> [a]
-- PRECOND: n >= 0
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (_ : xs) = sinLosPrimeros (n-1) xs

sinLosPrimeros' :: Int -> [a] -> [a]
sinLosPrimeros' 0 xs = xs
sinLosPrimeros' n xs = 
    if (null xs)
        then []
        else sinLosPrimeros' (n-1) (tail xs)

--3------------------------------------------------------
data Persona = P String Int
    deriving Show
eze = P "eze" 21
viejo = P "viejo" 99
nene = P "nene" 3


edad :: Persona -> Int
edad (P _ e) = e

{-3.1.1 Dados una edad y una lista de personas devuelve a 
las personas mayores a esa edad-}

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (p : ps) = 
    if (edad p) > n
        then p : mayoresA n ps
        else mayoresA n ps

{-3.1.2 Dada una lista de personas devuelve el promedio de
edad entre esas personas. Precondición: 
la lista al menos p osee una persona.-}

promedioEdad :: [Persona] -> Int
--PRECOND: la lista al menos posee una persona
promedioEdad [] = error "La lista al menos posee una persona"
promedioEdad [p] = edad p
promedioEdad ps = div (edadTotal ps) (longitud ps)

edadTotal :: [Persona] -> Int
edadTotal [] = 0
edadTotal (p : ps) = (edad p) + edadTotal ps

{-3.1.3 Dada una lista de personas devuelve la persona 
más vieja de la lista. Precondición : la lista al menos 
posee una persona.-}

elMasViejo :: [Persona] -> Persona
--PRECOND: la lista al menos posee una persona.
elMasViejo [] = error "La lista al menos posee una persona"
elMasViejo [p] = p 
elMasViejo (p : ps) = 
    if edad p > edad (elMasViejo ps)
        then p
        else elMasViejo ps

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

pikachu = ConsPokemon Agua 12
charmander = ConsPokemon Fuego 10
bulbasaur = ConsPokemon Planta 8

ash = ConsEntrenador "Ash" [pikachu, charmander, bulbasaur]
brock = ConsEntrenador "Brock" [bulbasaur, bulbasaur]


pokemonDe :: Entrenador -> [Pokemon]
pokemonDe (ConsEntrenador _ ps) = ps


{-3.2.1 Devuelve la cantidad de Pokémon que 
posee el entrenador -}

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = longitud ps

{-3.2.2 Devuelve la cantidad de Pokémon de determinado tipo 
que posee el entrenador.-}

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonDe' t ps

cantPokemonDe' :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDe' _ [] = 0
cantPokemonDe' t (p : ps) =
    if esIgual t (tipoDe p)
        then 1 + cantPokemonDe' t ps
        else cantPokemonDe' t ps

esIgual :: TipoDePokemon -> TipoDePokemon -> Bool
esIgual Agua Agua = True
esIgual Fuego Fuego = True
esIgual Planta Planta = True
esIgual _ _ = False

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _ )= t

{-3.2.3 Dados dos entrenadores, indica la cantidad de Pokemon de
cierto tipo, que le ganarían a los Pokemon del segundo 
entrenador.-}

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = 
    cuantosDeTipo_De_LeGananATodosLosDe_' t (pokemonDe e1) (pokemonDe e2)

cuantosDeTipo_De_LeGananATodosLosDe_' ::TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosDeTipo_De_LeGananATodosLosDe_' _ [] _ = 0
cuantosDeTipo_De_LeGananATodosLosDe_' t (p1 : ps1) ps2 =
     unoSiCeroSino (esIgual t (tipoDe p1) && leGanaATodos (tipoDe p1) ps2) + cuantosDeTipo_De_LeGananATodosLosDe_' t ps1 ps2

    

leGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaA Agua Fuego = True
leGanaA Fuego Planta = True
leGanaA Planta Agua = True
leGanaA _ _ = False

leGanaATodos :: TipoDePokemon -> [Pokemon] -> Bool
leGanaATodos _ [] = True
leGanaATodos t (p : ps) = (leGanaA t (tipoDe p)) && (leGanaATodos t ps )

{-3.2.4 Dado un entrenador, devuelve True si posee 
al menos un Pokémon de cada tip o posible-}

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = (cantPokemonDe Agua e) > 0 &&
                     (cantPokemonDe Fuego e) > 0 && 
                     (cantPokemonDe Planta e) > 0

data Seniority = Junior | SemiSenior | Senior
    deriving (Show, Eq)
data Proyecto = ConsProyecto String
    deriving (Show, Eq)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving (Show, Eq)
data Empresa = ConsEmpresa [Rol]
    deriving (Show, Eq)

proyecto1 = ConsProyecto "proyecto1"
proyecto2 = ConsProyecto "proyecto2"
proyecto3 = ConsProyecto "proyecto3"

rol1 = Developer Junior proyecto1
rol2 = Developer SemiSenior proyecto2
rol3 = Developer Senior proyecto3


roles = [rol1, rol2, rol2, rol3, rol3, rol3,rol3]

facebook = ConsEmpresa roles

--3-------------------------------------------------------------------------

{-3.1 Dada una empresa denota la lista de proyectos en los que trabaja,
 sin elementos repetidos.-}

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosDeRoles rs

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (r : rs) =
    if (esRepetido r rs)
        then proyectosDeRoles rs
        else (proyectoDeRol r) : proyectosDeRoles rs


proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p
proyectoDeRol (Management _ p) = p

esRepetido :: Eq a => a -> [a] -> Bool
esRepetido _ [] = False
esRepetido x (y : ys) =  x == y || esRepetido x ys


{-3.2 Dada una empresa indica la cantidad de 
desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.-}

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = losDevSenior' rs ps

losDevSenior' :: [Rol] -> [Proyecto] -> Int
losDevSenior' [] _ = 0
losDevSenior' (r : rs) ps =
    unoSiCeroSino (esDevSenior r && existeEn (proyectoDeRol r) ps) + losDevSenior' rs ps

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _ = False

existeEn :: Proyecto -> [Proyecto] -> Bool
existeEn _ [] = False
existeEn p' (p : ps) =
    p' == p || existeEn p' ps


{-3.3 Indica la cantidad de empleados que 
trabajan en alguno de los proyectosdados.-}

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = cantQueTrabajanEn' ps rs

cantQueTrabajanEn' :: [Proyecto] -> [Rol] -> Int
cantQueTrabajanEn' ps [] = 0 
cantQueTrabajanEn' ps (r : rs) =
    unoSiCeroSino (existeEn (proyectoDeRol r) ps) + cantQueTrabajanEn' ps rs

{-3.4 Devuelve una lista de pares que representa a los
proyectos (sinrepetir) junto con su cantidad de 
personas involucradas.-}

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e =
    asignadosPorProyecto' (proyectos e) e

asignadosPorProyecto' :: [Proyecto] -> Empresa -> [(Proyecto, Int)]
asignadosPorProyecto' [] _ = []
asignadosPorProyecto' (p : ps) e = 
    (p, cantQueTrabajanEn [p] e) : asignadosPorProyecto' ps e
