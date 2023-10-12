--a

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{-
    INV.REP: dada N S T R
    1- Todas las personas de R deben ser las mismas que T
    2- Si i es una clave de S, fromJust(lookUp i S) tiene sectorId i
    3- Si n es una clave de T, fromJust(lookUp n T) tiene nombre n
    4- Sea t un tripulante de la nave y t tiene asignado un sector, el sector es valor de S
    5- Si s es un sector de S y tiene asignado un tripulante, el tripulante es valor de T
    6- Sea t un tripulante, si t esta asignado a un sector, el id de ese sector debe estar en la lista de sectores de t
-}

-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S)
construir :: [SectorId] -> Nave
construir  ids =  N (mapaDeSectores ids) emptyM emptyH

-- Eficiencia: O(S Log(S))
mapaDeSectores :: [SectorId] -> (Map SectorId Sector)
mapaDeSectores [] = emptyM
                            --     k     v          (Map SectorId Sector) Log(K)
mapaDeSectores (id : ids) = assocM id (crearS id) (mapaDeSectores ids)

-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT nt rt (N s t r) = N s (agregarT nt rt t) r

--Eficiencia: Log(T)
agregarT :: Nombre -> Rango -> (Map Nombre Tripulante) -> (Map Nombre Tripulante)
                        -- k ----v------map Log(K)
agregarT nt rt t = assocM nt (crearT nt rt) t

--tripulanteN: O(T Log(T))