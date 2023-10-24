import PriorityQueue

heapSort :: Ord a => [a] -> [a]
heapSort xs = heapSortAux (foldr insertPQ emptyPQ xs)

heapSortAux :: Ord a => PriorityQueue a -> [a]
heapSortAux pq =
    if isEmptyPQ pq then [] else findMinPQ pq : heapSortAux (deleteMinPQ pq)

