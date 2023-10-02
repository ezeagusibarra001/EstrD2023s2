module MultiSet(
    MultiSet,
    emptyMS
) where

import Map

data MultiSet a = MS (Map a Int) deriving Show

emptyMS :: MultiSet a
emptyMS = MS emptyM