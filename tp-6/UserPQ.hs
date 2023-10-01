import PriorityQueue

heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort (x : xs) =
    