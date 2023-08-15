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

-- sumar 5 5
-- sucesor 9
-- maxDelPar (10, 5)
-- sumar 4 6

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
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

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
negar False = True

--3b)