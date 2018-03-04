
module SuffixTree.Util where

import           Data.Set hiding (null)
import           Data.Text as Text
import           Protolude hiding (head, null)

tail :: [a] -> [a]
tail []       = []
tail (_ : xs) = xs

removeHeads :: [Text] -> [Text]
removeHeads []        = []
removeHeads(xs : xss)
    | null xs         = removeHeads xss
    | otherwise       = Text.tail xs : removeHeads xss

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


headEq :: Char -> Text -> Bool
headEq _ xs | null xs  = False
headEq a xs = a == head xs


removeDuplicates :: Text -> Text
removeDuplicates ""  =  ""
removeDuplicates t   =  x `cons` removeDuplicates withoutX
    where x = Text.head t
          withoutX = Text.filter (\y -> x /= y) (Text.tail t)


listify :: (a, a) -> [a]
listify (a, b) = [a, b]


-- nub :: Text -> Text
-- nub = Text.foldr insert


