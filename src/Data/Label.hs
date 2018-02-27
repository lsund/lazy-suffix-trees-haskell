
module Data.Label where

import Protolude

import Data.List  as List


data Label a = Label
    { _mark :: [a]
    , _len  :: Int
    } deriving (Eq, Show)


fromList :: [a] -> Label a
fromList xs = Label xs (length xs)


take :: Int -> Label a -> Label a
take n (Label xs _) = Label xs n


tail :: Label a -> Label a
tail (Label mark n) = Label (List.tail mark) n


shrink :: Label a -> Label a
shrink (Label mark n) = Label (List.tail mark) (pred n)


empty :: Label a -> Label a
empty lbl = Label (_mark lbl) 0


isEmpty :: Label a -> Bool
isEmpty (Label _ 0) = True
isEmpty _           = False



