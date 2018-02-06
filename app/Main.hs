module Main where

import  Protolude            hiding (filter)
import  Data.Text            as T   (unpack, filter)
import  Data.List            hiding (filter)

import  LazyTree.Functional
import  Draw

-------------------------------------------------------------------------------
-- Program

removeNewLines :: Text -> Text
removeNewLines = filter (/= '\n')

runFilePath :: FilePath -> IO ()
runFilePath path = do
    cont <- readFile path
    let s = unpack (removeNewLines cont)
    drawPretty $ lazyCST (nub s) s


main :: IO ()
main =
    -- do
    -- args <- getArgs
    -- print args
    runFilePath "data/advAlg"

