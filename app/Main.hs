module Main where

import qualified Data.Text.Lazy                         as T
import           Data.Text.Lazy.IO                      (appendFile, readFile,
                                                         writeFile)
import           Protolude                              hiding (Text,
                                                         appendFile, readFile,
                                                         writeFile)

import           SuffixTree.Algorithm.LazyTreeCountSort
import           SuffixTree.Algorithm.Search

-- Specify input file here
datafile :: FilePath
datafile = "testdata/members.xml"

main :: IO ()
main = do
    print $ "Creating a suffix tree for " ++ datafile
    content <- readFile datafile
    let nos = T.lines content
        tree = lazyTreeCount content
    print "Done."


