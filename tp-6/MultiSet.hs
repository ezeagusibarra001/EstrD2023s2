module MultiSet(
    MultiSet,
    emptyMS
) where

import Map

data MultiSet a = MS (Map a Int) deriving Show

{-
    INV.REP
    dado MS (m)
    1) lookupM x m, sin importar x,no puede ser negativo

-}

multi = MS (assocM 1 2 (assocM 2 3 (assocM 3 4 emptyM)))
multi2 = MS (assocM 5 5 (assocM 2 3 emptyM))

--Constante
emptyMS :: MultiSet a
emptyMS = MS emptyM

--Lineal
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS k (MS map) = MS (agregarOcurrencia k map)

agregarOcurrencia :: Ord a => a -> Map a Int -> Map a Int
agregarOcurrencia k map =
    let maybeV = lookupM k map
    in if isNothing maybeV
        then assocM k 1 map 
        else assocM k (valor maybeV + 1) map

valor :: Maybe v -> v
valor (Just n) = n

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS k (MS map) =
    let maybeV = lookupM k map
    in if isNothing maybeV
        then 0
        else valor maybeV

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS map) ms = unificarA (keys map) map ms

--Lineal
unificarA :: Ord a => [a] -> Map a Int -> MultiSet a -> MultiSet a
unificarA [] _ ms = ms
unificarA (k : ks) map ms = agregarNueva k (valor(lookupM k map)) (unificarA ks map ms)

agregarNueva :: Ord a =>  a -> Int -> MultiSet a -> MultiSet a
agregarNueva k n ms = 
    let oldN = ocurrencesMS k ms
    in MS (assocM k (n + oldN) (mapMS ms))

mapMS :: MultiSet a -> Map a Int
mapMS (MS map) = map

multiSetToList :: Ord a => Eq a => MultiSet a -> [(a, Int)]
multiSetToList ms = multiSetToList' (keys (mapMS ms)) ms

multiSetToList' :: Ord a => [a] -> MultiSet a -> [(a, Int)]
multiSetToList' [] ms = []
multiSetToList' (k : ks) ms = (k, ocurrencesMS k ms) : multiSetToList' ks ms
