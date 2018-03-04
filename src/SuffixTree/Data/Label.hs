
module SuffixTree.Data.Label where

import qualified Data.Text.Lazy as T
import           Protolude


data Label = Label
    { _mark :: T.Text
    , _len  :: Int64
    } deriving (Eq, Show)


fromList :: T.Text -> Label
fromList xs = Label xs (T.length xs)

take :: Int64 -> Label -> Label
take n (Label xs _) = Label xs n


tail :: Label -> Label
tail (Label mark n) = Label (T.tail mark) n


shrink :: Label -> Label
shrink (Label mark n) = Label (T.tail mark) (pred n)


grow :: Label -> Label
grow (Label mark n) = Label mark (succ n)


empty :: Label -> Label
empty lbl = Label (_mark lbl) 0


isEmpty :: Label -> Bool
isEmpty (Label _ 0) = True
isEmpty _           = False


rest :: Label -> Label
rest lbl = fromList (T.drop (_len lbl) (_mark lbl))


drop :: Label -> Label -> Label
drop lbl lbl' = fromList (T.drop (_len lbl) (_mark lbl'))


compareFirst :: Label -> Label -> Ordering
compareFirst (Label xs _) (Label ys _) = T.head xs `compare` T.head ys
