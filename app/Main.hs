module Main where

import  Protolude            hiding (filter)
import  Data.Text            as T   (unpack, filter)
import  Data.List            hiding (filter)

import  LazyTree.Functional
import  Draw
import  Search

-------------------------------------------------------------------------------
-- Program

removeNewLines :: Text -> Text
removeNewLines = filter (/= '\n')

treeFromFile :: FilePath -> IO (STree Char)
treeFromFile path = do
    cont <- readFile path
    let s = unpack (removeNewLines cont)
    return $ lazyCST (nub s) s


main :: IO ()
main =
    do
    t <- treeFromFile "data/advAlg"
    drawPretty t
    print $ search t "ab"


