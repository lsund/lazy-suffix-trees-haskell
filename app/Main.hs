module Main where

import  Protolude            hiding (filter)
import  Data.Text            as T   (unpack, filter)
import  Data.List            hiding (filter)

-- import Analyze
import  LazyTree.Functional
-- import  Draw
import  Search

-------------------------------------------------------------------------------
-- Program

removeNewLines :: Text -> Text
removeNewLines = filter (/= '\n')

treeFromFile :: FilePath -> IO (STree Char)
treeFromFile path = do
    cont <- readFile path
    let s = unpack (removeNewLines cont)
    print s
    return $ lazyCST (nub s) s


-- abaababa
main :: IO ()
main =
    do
    cont1 <- readFile "data/book/data.xml"
    alpha <- readFile "data/book/alpha.txt"
    -- t <- treeFromFile "data/advAlg"
    -- drawPretty t
    -- cont <- readFile "data/advAlg"
    print $ indices (unpack alpha) (unpack cont1) "ENTSCHEIDUNG"


