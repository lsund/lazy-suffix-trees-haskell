module Main where

import qualified Data.Text.Lazy                           as T
import           Data.Text.Lazy.IO                        (readFile, appendFile, writeFile)
import           Protolude                                hiding (Text,
                                                           readFile, appendFile, writeFile)

import           SuffixTree.Algorithm.LazyTreeCountSort
import           SuffixTree.Algorithm.Search

datafile :: FilePath
datafile = "data/book/data.xml"
alphafile :: FilePath
alphafile = "data/book/alpha.txt"

main :: IO ()
main = do
    content <- readFile datafile
    let nos = T.lines content
        tree = lazyTreeCount content
    print "todo"


