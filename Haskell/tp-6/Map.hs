module Map (
	Map,
	emptyM,
	assocM,
	lookupM,
	deleteM,
	keys
) where

data Map k v = M [(k, v)] deriving Show

maps = M [("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5)]
-- Invariantes:
-- 1. Las k no se pueden repetir. 

emptyM :: Map k v
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M (asociar k v kvs)

asociar :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
asociar k v [] = [(k, v)]
asociar k v ((k', v') : kvs) = 
    if k == k'
        then (k, v) : kvs
        else (k', v') : asociar k v kvs

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = buscar k kvs

buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k [] = Nothing
buscar k ((k', v') : kvs) = 
    if k == k'
        then Just v'
        else buscar k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (borrar k kvs)

borrar :: Eq k => k -> [(k, v)] -> [(k,v)]
borrar k [] = []
borrar k ((k', v') : kvs) =
    if k == k'
        then kvs
        else (k', v') : borrar k kvs

keys :: Eq k => Map k v -> [k]
keys (M xs) = claves xs

claves :: Eq k => [(k, v)] -> [k]
claves [] = []
claves ((k, v) : kvs) = k : claves kvs