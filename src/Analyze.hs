
module Analyze where

import Protolude

import Util

alphabet :: Text -> Text
alphabet = removeDuplicates

writeAlphabet :: FilePath -> IO ()
writeAlphabet path = do
    cont <- readFile (path ++ "/data.xml")
    writeFile (path ++ "/alpha.txt") (alphabet cont)
