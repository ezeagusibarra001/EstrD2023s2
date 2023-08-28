-- 1 -----------------------------------------------

-- 1a) Dado un número devuelve su sucesor

sucesor :: Int -> Int
sucesor x = x + 1

--1b) Dados dos números devuelve su suma utilizando la op eración +.

sumar :: Int -> Int -> Int
sumar x y = x + y

{-1c)
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda comp onente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista p or Haskell.-}

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

--1d) Dado un par de números devuelve el mayor de estos.

maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) = max x y

-- 2 ---------------------------------------------
{-
De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expre-
sión a to das las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))
-}

-- maxDelPar (divisionYResto (sumar (sucesor 4) 15) 2)

-- maxDelPar (divisionYResto (sumar (sucesor 9) 30) 4)

-- maxDelPar (divisionYResto (sumar (sucesor 19) 60) 8)

-- maxDelPar (divisionYResto (sumar (sucesor 39) 120) 16)

-- 3 ---------------------------------------------

data Dir = Norte | Sur | Este | Oeste
    deriving Show

--1a) Dada una dirección devuelve su opuesta.

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este

-- 1b) Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

-- 1c) Dada una dirección devuelve su siguiente, en sentido horario, y sup oniendo que no existe la siguiente dirección a Oeste.

siguiente :: Dir -> Dir
-- precondicion: la direccion no es Oeste
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente _ = error "La direccion no tiene siguiente"

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

{- 2a) Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después-}

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--2b) Dado un día de la semana indica si comienza con la letra M.

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

{-2c)Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deb erían ser no más de 9 casos).-}

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = posicionDelDia d1 > posicionDelDia d2

posicionDelDia :: DiaDeSemana -> Int
posicionDelDia Lunes = 0
posicionDelDia Martes = 1
posicionDelDia Miercoles = 2
posicionDelDia Jueves = 3
posicionDelDia Viernes = 4
posicionDelDia Sabado = 5
posicionDelDia Domingo = 6

--2d) Dado un día de la semana indica si no es ni el primer ni el ultimo dia.

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--3---------------------------------------------------------

--3a)Dado un b o oleano, si es True devuelve False, y si es False devuelve True.

negar :: Bool -> Bool
negar True = False
negar _ = True

{-3b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True-}

implica :: Bool -> Bool -> Bool
implica True b2 = b2
implica _ _ = True

--3c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False.

yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien True b2 = b2

--3d) Dados dos b o oleanos si alguno de ellos es True devuelve True, sino devuelve False.

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b2 = b2

--4-------------------------------------------

data Persona = P String Int 
    deriving Show

eze = P "Ezequiel" 21
viejo = P "Viejo" 99

--4.1.a) Devuelve el nombre de una persona

nombre :: Persona -> String
nombre (P n _) = n

--4.1.b) Devuelve la edad de una persona

edad :: Persona -> Int
edad (P _ e) = e

--4.1.c) Aumenta en uno la edad de la persona.

crecer :: Persona -> Persona
crecer (P n e) = P n (sucesor e)

--4.1.d) Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n p = P n (edad p)

--4.1.f) Dadas dos personas indica si la primera es mayor que la segunda

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

--4.1.g) Dadas dos personas devuelve a la persona que sea mayor.

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 =
    if edad p1 > edad p2
        then p1
        else p2

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
data Pokemon = Poke TipoDePokemon Int
    deriving Show

data Entrenador = Ent String Pokemon Pokemon
    deriving Show
pikachu = Poke Fuego 85
bulbasor = Poke Planta 50
r2d2 = Poke Agua 30

rokero = Ent "Rockero" pikachu r2d2

pokemaniaco = Ent "Pokemaniaco" r2d2 bulbasor

{-4.2.a) Dados dos Pokémon indica si el 
primero, en base al tipo, es superior al 
segundo. Agua supera a fuego, 
fuego a planta y planta a agua. 
Y cualquier otro caso es falso.
-} 

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = leGanaA (tipoDe p1) (tipoDe p2)

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (Poke t _ )= t

leGanaA :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaA Agua Fuego = True
leGanaA Fuego Planta = True
leGanaA Planta Agua = True
leGanaA _ _ = False

{-4.2.b)
Devuelve la cantidad de Pokémon de 
determinado tipo que posee el entrenador.-}

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (Ent _ p1 p2) =
    unoSiCeroSino (esElMismo t (tipoDe p1)) +
    unoSiCeroSino (esElMismo t (tipoDe p2))

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

esElMismo :: TipoDePokemon -> TipoDePokemon -> Bool
esElMismo Agua Agua = True
esElMismo Fuego Fuego = True
esElMismo Planta Planta = True
esElMismo _ _ = False

{-4.2.c)
Dado un par de entrenadores, 
devuelve a sus Pokémon en una lista-}

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon  (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent _ p1 p2) = [p1,p2]

--5----------------------------------------------

--5a) Dado un elemento de algún tipo devuelve ese mismo elemento

loMismo :: a -> a
loMismo x = x

--5b) Dado un elemento de algún tip o devuelve el número 7

siempreSiete :: a -> Int
siempreSiete _ = 7

--5c) Dadas una tupla, invierte sus comp onentes.

swap :: (a,b) -> (b, a)
swap (x, y) = (y, x)

{-  Responda la siguiente pregunta: 
    Por qué estas funciones son polimórficas?

    Porque no importa el tipo de dato que reciban,
    siempre devuelven el mismo tipo de dato.  
-}

--6----------------------------------------------\

{-6a)   
    Dada una lista de elementos, si es 
    vacía devuelve True, sino devuelve False.
    Definida en Haskell como null. -}

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

-- 6b) Dada una lista devuelve su primer elemento.

elPrimero :: [a] -> a
-- precondicion: la lista no debe estar vacia
elPrimero (x: xs) = x

-- 6c) Dada una lista devuelve esa lista menos el primer elemento.

sinElPrimero :: [a] -> [a]
-- precondicion: la lista no debe estar vacia
sinElPrimero (x: xs) = xs

{- 6d) Dada una lista devuelve un par, donde
la primera componente es el primer elemento de la
lista, y la segunda componente es esa 
lista pero sin el primero-}

splitHead :: [a] -> (a, [a])
-- precondicion: la lista no debe estar vacia
splitHead (x: xs) = (x, xs)
