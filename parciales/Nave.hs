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
-- Eficiencia: O(S Log (S))
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
ingresarT nt rt (N s t r) = N s (agregarTM nt rt t) (agregarTH nt rt r) 

--Eficiencia: Log(T)
agregarTM :: Nombre -> Rango -> (Map Nombre Tripulante) -> (Map Nombre Tripulante)
                        -- k ----v------map Log(K)
agregarTM nt rt t = assocM nt (crearT nt rt) t

agregarTH :: Nombre -> Rango -> (MaxHeap Tripulante) -> (MaxHeap Tripulante)
                    --O(Log M)
agregarTH nt rt r = insertH (crearT nt rt) r

-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N mapS mapT maxHeap r) = sectoresDe n mapT

sectoresDe :: Nombre -> (Map Nombre Tripulante) -> Set SectorId
                            -- log k
sectorDe n mapT = let t in lookupM n mapT
                    --O(1)
                  sectoresT (fromJust t)
                
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe v -> v
-- precond: v no es Nothng
fromJust (Just v) = v


-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
                                                            -- O(1)    O(Log S)
datosDeSector id (N mapS mapT maxHeapR) = datosDelSectorId (fromJust (lookupM id mapS))

--O(1)
datosDelSectorId :: Sector -> (Set Nombre, [Componente])
                        -- O(1)         O(1)
datosDelSectorId s = ((tripulantesS s), (componentesS s))


-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(T Log(T))
 tripulantesN :: Nave -> [Tripulante]
 tripulantesN (N mapS mapT maxHeapR) = tripulantesH maxHeapR

tripulantesH :: (MaxHeap Tripulante) -> [Tripulante]
tripulantesH maxHeapR =
            if (isEmptyH maxHeapR)
                then []
                else (maxH maxHeapR) : (deleteMaxH tripulantesH)

-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Precondicion: Existe ese sectorId en la nave
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector ls id (N mapS mapT maxHeapR) = N (agregarAMapS ls id mapS) mapT maxHeapR

--O(log S)
agregarAMapS ::  [Componente] -> SectorId -> (Map SectorId Sector) -> (Map SectorId Sector)
                            --                      log k               log k
agregarAMapS ls id mapS = agregarAS ls (fromJust(lookupM id mapS)) (deleteM id mapS)

agregarAS ::[Componente] -> Sector -> (Map SectorId Sector) -> (Map SectorId Sector)
agregarAS [] s mapS       = assocM (sectorId s) s mapS 
agregarAS (c : cs) s mapS = agregarAS cs (agregarC c s) mapS

-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n id (N s t r) = N s (asignarAT n id t) (asignarAR n id r)

asignarAT :: Nombre -> SectorId -> (Map Nombre Tripulante) -> (Map Nombre Tripulante)
asignarAT n id mapT = let tripulante in fromJust(lookupM n mapT)
                    --log s
                      assocM n (asignarS id tripulante) mapT
--T log T
asignarAR :: Nombre -> SectorId -> (MaxHeap Tripulante) -> (MaxHeap Tripulante)
asignarAR n id maxHeapR =
        if (isEmptyH maxHeapR)
            then emptyH
                --log m
            else insertH (asignarATR n id (maxH maxHeapR)) (asignarAR n id (deleteMaxH maxHeapR))

--Log T
asignarATR :: Nombre -> SectorId -> Tripulante -> Tripulante
asignarATR n id t =
    if (nombre t) === n
        then asignarS id t
        else t

-- USUARIIO

-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
--Eficiencia: O(T log T + T log T)
sectores :: Nave -> Set SectorId
sectores nave = sectoresDeTripulantes (tripulantesN nave)

-- O(N log N)
sectoresDeTripulantes ::  [Tripulante] -> Set SectorId
sectoresDeTripulantes []       = emptyS
sectoresDeTripulantes (t : ts) = unionS (sectoresT t) (sectoresDeTripulantes ts)

-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
-- O(T log T + Log N)
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados nave = sinSectoresAsignadosTripulante (tripulantesN nave)

sinSectoresAsignadosTripulante :: [Tripulante] -> [Tripulante]
sinSectoresAsignadosTripulante []       = emptyS
sinSectoresAsignadosTripulante (t : ts) = 
                                if (sizeS (sectoresT t) == 0)
                                    then (addS t) : (sinSectoresAsignadosTripulante ts)
                                    else sinSectoresAsignadosTripulante ts

-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles :: Nave -> [Barril]
barriles nave = barrilesDeTripulantes (setToList (sectores nave)) nave

barrilesDeTripulantes :: [SectorId] -> Nave -> [Barril]
barrilesDeTripulantes [] nave      = []
barrilesDeTripulantes (id : ids) nave = (barrilesDeId id nave) ++ barrilesDeTripulantes ids nave

barrilesDeId :: SectorId -> Nave -> [Barril]
barrilesDeId id nave = obtenerBarril (datosDeSector id nave)

obtenerBarril :: (Set Nombre, [Componente]) -> [Barril]
obtenerBarril (n, []) = []
obtenerBarril (n, (c : cs)) = barrilesDeC c ++ obtenerBarri n cs

barrilesDeC :: Componente -> [Barril]
barrilesDeC (Almacen bs) = bs
barrilesDeC _ = []