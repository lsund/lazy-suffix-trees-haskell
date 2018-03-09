
module SuffixTree.Util where

import qualified Data.List      as L
import           Data.MultiSet  as MS
import           Data.Text.Lazy (Text, cons)
import qualified Data.Text.Lazy as T
import           Prelude        (String)
import           Protolude      hiding (Text)


tail :: Text -> Text
tail "" = ""
tail xs = T.tail xs

removeHeads :: [Text] -> [Text]
removeHeads []        = []
removeHeads("" : xss) = removeHeads xss
removeHeads(xs : xss) = T.tail xs : removeHeads xss

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


fstEq :: Eq a => (a, b) -> (a, c) -> Bool
fstEq (x, _) (y, _) = x == y


headEq :: Char -> Text -> Bool
headEq _ "" = False
headEq a xs = a == T.head xs


removeDuplicates :: Text -> Text
removeDuplicates ""  =  ""
removeDuplicates t   =  x `cons` removeDuplicates withoutX
    where x = T.head t
          withoutX = T.filter (\y -> x /= y) (T.tail t)


listify :: (a, a) -> [a]
listify (a, b) = [a, b]

heads :: [Text] -> String
heads []       = []
heads [""]     = []
heads (x : xs) = T.head x : heads xs

countingSort :: [(Char, Text)] -> [[(Char, Text)]]
countingSort = L.groupBy fstEq . MS.toAscList . MS.fromList
