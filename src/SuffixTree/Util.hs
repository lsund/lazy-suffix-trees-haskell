
module SuffixTree.Util where

import           Data.Text as Text
import           Protolude hiding (head)

tail :: Text -> Text
tail ""       = ""
tail xs = Text.tail xs

removeHeads :: [Text] -> [Text]
removeHeads []        = []
removeHeads("" : xss) = removeHeads xss
removeHeads(xs : xss) = Text.tail xs : removeHeads xss

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


headEq :: Char -> Text -> Bool
headEq _ ""  = False
headEq a xs  = a == head xs


removeDuplicates :: Text -> Text
removeDuplicates ""  =  ""
removeDuplicates t   =  x `cons` removeDuplicates withoutX
    where x = head t
          withoutX = Text.filter (\y -> x /= y) (Text.tail t)


listify :: (a, a) -> [a]
listify (a, b) = [a, b]
