
module SuffixTree.Util where

import           Control.Arrow       ((&&&))
import qualified Data.List           as L
import           Data.Text.Lazy      (Text, cons)
import qualified Data.Text.Lazy      as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
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

third :: (a, b, c) -> c
third (_, _, x) = x

heads :: [Text] -> String
heads []       = []
heads [""]     = []
heads (x : xs) = T.head x : heads xs


groupVec :: Vector Text -> Vector [Text]
groupVec xs =
    -- third $
        let (_, b, c) =
                foldr
                    (\t (c, g, acc) ->
                        if T.head t == c
                            then (c, t : g, acc)
                            else (T.head t, [t], g `V.cons` acc))
                    ('v', [], empty) xs
                    -- Which first element here?
        in b `V.cons` c


splitSuffixes :: [Text] -> Vector Text
splitSuffixes t = V.fromList $ foldr splitJoin [] t
    where
        splitJoin "" acc = acc
        splitJoin x acc = x : acc

countingSort :: Vector Text -> [[Text]]
countingSort = L.groupBy fstEq . V.toList . countingSortV


countingSortV :: Vector Text -> Vector Text
countingSortV input = V.create $ do
    let lo = ord $ V.minimum $ T.head <$> input
        hi = ord $ V.maximum $ T.head <$> input
    -- let lo = 9                                        -- For the specific text
    --     hi = 252

    offsets <- V.thaw . V.prescanl (+) 0 $ V.create $ do
        counts <- MV.replicate (hi - lo + 1) 0
        V.forM_ input $ \x ->
            MV.modify counts succ (ord (T.head x) - lo)
        return counts

    output <- MV.new (V.length input)

    V.forM_ input $ \x -> do
        let i = ord $ T.head x
        ix <- MV.read offsets (i - lo)
        MV.write output ix x
        MV.modify offsets succ (i - lo)

    return output


