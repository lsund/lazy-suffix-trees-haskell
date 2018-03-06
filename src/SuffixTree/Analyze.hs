
module SuffixTree.Analyze where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy (Text, append)
import           Protolude       hiding (Text)

import           SuffixTree.Util

alphabet :: Text -> Text
alphabet = removeDuplicates

writeAlphabet :: FilePath -> IO ()
writeAlphabet path = do
    cont <- TIO.readFile (path ++ "/data.xml")
    TIO.writeFile (path ++ "/alpha.txt") (alphabet cont)

