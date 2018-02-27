
module Data.Label where

import Protolude


data Label a = Label
    { _mark :: [a]
    , _len  :: Int
    } deriving (Eq, Show)

full :: [a] -> Label a
full xs = Label xs (length xs)

take :: Int -> Label a -> Label a
take n (Label xs _) = Label xs n

