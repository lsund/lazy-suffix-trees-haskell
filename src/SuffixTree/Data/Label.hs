
module SuffixTree.Data.Label where

import           Data.Text as Text
import           Protolude hiding (length, head)


data Label = Label
    { _mark :: Text
    , _len  :: Int
    } deriving (Eq, Show)


fromList :: Text -> Label
fromList xs = Label xs (length xs)


take :: Int -> Label -> Label
take n (Label xs _) = Label xs n


tail :: Label -> Label
tail (Label mark n) = Label (Text.tail mark) n

shrink :: Label -> Label
shrink (Label mark n) = Label (Text.tail mark) (pred n)

grow :: Label -> Label
grow (Label mark n) = Label mark (succ n)


empty :: Label -> Label
empty lbl = Label (_mark lbl) 0


isEmpty :: Label -> Bool
isEmpty (Label _ 0) = True
isEmpty _           = False


rest :: Label -> Label
rest lbl = fromList (Text.drop (_len lbl) (_mark lbl))


drop :: Label -> Label -> Label
drop lbl lbl' = fromList (Text.drop (_len lbl) (_mark lbl'))


compareFirst :: Label -> Label -> Ordering
compareFirst (Label xs _) (Label ys _) = head xs `compare` head ys
