import Map

-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v
-- lookupM :: Eq k => k -> Map k v -> Maybe v
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Eq k => Map k v -> [k]

maps = assocM "a" 1 (assocM "b" 2 (assocM "c" 3 (assocM "d" 4 (assocM "e" 5 emptyM))))


valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = maybeValores (keys m) m 

maybeValores :: Eq k => [k] -> Map k v -> [Maybe v]
maybeValores [] m = []
maybeValores (k:ks) m = lookupM k m : maybeValores ks m

valores :: Eq k => Map k v -> [v]
valores m = obtenerValores (keys m) m

obtenerValores :: Eq k => [k] -> Map k v -> [v]
obtenerValores [] m     = []
obtenerValores (k:ks) m = 
	valor (lookupM k m) : obtenerValores ks m

-- Parcial cuando es Nothing
valor :: Maybe v -> v
valor Nothing = error "no se obtener un valor"
valor (Just x) = x