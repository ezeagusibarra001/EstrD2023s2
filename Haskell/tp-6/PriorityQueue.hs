
module PriorityQueue(
    PriorityQueue,
    emptyPQ,
    isEmptyPQ,
    insertPQ,
    findMinPQ,
    deleteMinPQ
) where

data PriorityQueue a = PQ [a] deriving Show

emptyPQ :: PriorityQueue a
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (x : xs)
 
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = minimum xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (borrar (minimum xs) xs)

borrar :: Eq a => a -> [a] -> [a]
borrar _ [] = []
borrar x (y : ys) =
    if x == y then ys else y : borrar x ys

-- pq = PQ [99,3,4,7,8]