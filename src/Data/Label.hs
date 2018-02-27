
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

rest :: Label a -> Label a
rest lbl = fromList (List.drop (_len lbl) (_mark lbl))

drop :: Label a -> Label a -> Label a
drop lbl lbl' = fromList (List.drop (_len lbl) (_mark lbl'))


compareFirst :: Ord a => Label a -> Label a -> Ordering
compareFirst (Label (x : _) _) (Label (y : _) _) = x `compare` y

