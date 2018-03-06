
module SuffixTree.Util where

import           Data.Array
import           Data.Text.Lazy (Text, cons)
import qualified Data.Text.Lazy as T
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

-- countingSort :: (Ix n) => [n] -> n -> n -> [n]
countingSort l lo hi = [replicate times n | (n, times) <- counts]
  where counts = assocs (accumArray (+) 0 (lo, hi) [(i, 1) | i <- l])

