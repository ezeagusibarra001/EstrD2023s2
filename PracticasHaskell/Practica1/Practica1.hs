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
-- 2a)