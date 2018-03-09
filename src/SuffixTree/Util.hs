
module SuffixTree.Util where

import           Control.Arrow       ((&&&))
import qualified Data.List           as L
import           Data.Text.Lazy      (Text, cons)
import qualified Data.Text.Lazy      as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector
import           Prelude             (String)
import           Protolude           hiding (Text)



tail :: Text -> Text
tail "" = ""
tail xs = T.tail xs

removeHeads :: [Text] -> [Text]
removeHeads []        = []
removeHeads("" : xss) = removeHeads xss
removeHeads(xs : xss) = T.tail xs : removeHeads xss

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


fstEq :: Text -> Text -> Bool
fstEq xs ys = T.head xs == T.head ys


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


groupVec = undefined


splitSuffixes :: [Text] -> Vector Text
splitSuffixes t = Vector.fromList $ foldr splitJoin [] t
    where
        splitJoin "" acc = acc
        splitJoin x acc = x : acc

countingSort :: Vector Text -> [[Text]]
countingSort = L.groupBy fstEq . V.toList . countingSortV


countingSortV :: Vector Text -> Vector Text
countingSortV input = Vector.create $ do
    let lo = ord $ Vector.minimum $ T.head <$> input
        hi = ord $ Vector.maximum $ T.head <$> input

    offsets <- Vector.thaw . Vector.prescanl (+) 0 $ Vector.create $ do
        counts <- MVector.replicate (hi - lo + 1) 0
        Vector.forM_ input $ \x ->
            MVector.modify counts succ ((ord $ T.head x) - lo)
        return counts

    output <- MVector.new (Vector.length input)
    Vector.forM_ input $ \x -> do
        let i = ord $ T.head x
        ix <- MVector.read offsets (i - lo)
        MVector.write output ix x
        MVector.modify offsets succ (i - lo)

    return output


