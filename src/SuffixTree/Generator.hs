
module SuffixTree.Generator where

import Data.List.Split (chunksOf)
import System.Random
import Protolude
import Prelude (String)
import Data.Text (pack, append)

randomString :: IO String
randomString = do
  g <- newStdGen
  return $ randomRs ('a','b') g

generateRandomText :: Int -> FilePath -> IO ()
generateRandomText len path =
    randomString >>= (writeFile path . pack . take len)

-- number-of-patterns pattern-length filepath
generatePatterns :: Int -> Int -> FilePath -> IO ()
generatePatterns np pl path = do
    writeFile path ""
    text <-  randomString
    mapM_ (\p -> appendFile path (pack p `append` "\n")) $ chunksOf pl $ take (np * pl) text


