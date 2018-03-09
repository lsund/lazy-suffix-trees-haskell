
module Util where

import qualified Data.List as L
import qualified Data.Text.Lazy                           as T
import qualified Data.Vector                              as V
import           Prelude                                  (String)
import           Protolude                                hiding (Text)
import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Util
import           Test.QuickCheck

prop_countsort :: [(Char, String)] -> Bool
prop_countsort xs
    | all (\x -> x == L.head (map fst xs)) (map fst xs) = True
    | otherwise = countingSort t == countingSortV vx
    where
        t = map (\(c, x) -> (c, T.pack x)) xs
        vx = V.fromList t
