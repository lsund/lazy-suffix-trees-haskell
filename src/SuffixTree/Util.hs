
module SuffixTree.Util where

import           Control.Arrow       ((&&&))
import qualified Data.List           as L
import qualified Data.MultiSet       as MS
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

splitSuffixes :: [Text] -> [(Char, Text)]
splitSuffixes = map (T.head &&& T.tail)

countingSort :: [(Char, Text)] -> [[(Char, Text)]]
countingSort = L.groupBy fstEq . MS.toAscList . MS.fromList

splitSuffixesV :: [Text] -> Vector (Char, Text)
splitSuffixesV t = Vector.fromList $ foldr splitJoin [] t
    where
        splitJoin "" acc = acc
        splitJoin x acc = (T.head x, T.tail x) : acc

countingSortV :: Vector (Char, Text) -> [[(Char, Text)]]
countingSortV = L.groupBy fstEq . V.toList . countingSortVector

countingSortVector :: Vector (Char,a) -> Vector (Char,a)
countingSortVector input
    | null input = input
    | otherwise = Vector.create $ do
  let lo = Vector.minimum $ fst <$> input
      hi = Vector.maximum $ fst <$> input

  offsets <- Vector.thaw . Vector.prescanl (+) 0 $ Vector.create $ do
    counts <- MVector.replicate (ord hi - ord lo + 1) 0
    Vector.forM_ input $ \(i,_) ->
      MVector.modify counts succ (ord i - ord lo)
    return counts

  output <- MVector.new (Vector.length input)
  Vector.forM_ input $ \p@(i,_) -> do
    ix <- MVector.read offsets (ord i - ord lo)
    MVector.write output ix p
    MVector.modify offsets succ (ord i - ord lo)

  return output


