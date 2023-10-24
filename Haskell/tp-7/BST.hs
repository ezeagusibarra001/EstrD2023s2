module BST(
    BST,
    belongsBST,
    insertBST
) where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

data BST a = BST (Tree a) deriving Show

treeBalanciado = NodeT 2
                    (NodeT 1 EmptyT EmptyT)
                    (NodeT 5 EmptyT EmptyT)

{-
    INV.REP:
     en (S t), t cumple ser un BST
-}

-- belongsBST y ti || belongsBST y td O(n)

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST y EmptyT = False
belongsBST y (NodeT x ti td) = y == x || 
                            if (y < x)
                                then belongsBST y ti
                                else belongsBST y td
-- O(n)

insertBST :: Ord a => a -> Tree a -> Tree a
insertarBST y EmptyT =
insertarBST y (NodeT x ti td) =


