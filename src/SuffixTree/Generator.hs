
module SuffixTree.Generator where

import System.Random
import Protolude
import Prelude (String)
import Data.Text (pack)

g2 :: IO String
g2 = do
  g <- newStdGen
  return $ randomRs ('a','b') g

generateRandomText :: Int -> IO ()
generateRandomText len = g2 >>= (writeFile "data/test.txt" . pack . take len)

