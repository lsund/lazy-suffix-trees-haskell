
module SuffixTree.Draw where

import qualified Data.Text                                as Text
import           Data.Tree
import           Data.Tree.Pretty
import           Prelude                                  (String)
import           Protolude

import           SuffixTree.Algorithm.Common
import           SuffixTree.Algorithm.LazyTree.Functional
import           SuffixTree.Data.Label                    hiding (take)
import           SuffixTree.Data.SuffixTree
import           SuffixTree.Reader

-------------------------------------------------------------------------
-- Draw


drawST :: (Tree String -> String) -> STree -> IO ()
drawST drawFun = putStr . drawFun . map edgeLabel . toTree
    where
        edgeLabel (Label s l) = take l (Text.unpack s)

draw :: STree -> IO ()
draw = drawST drawTree

drawPretty :: STree -> IO ()
drawPretty = drawST drawVerticalTree

drawFile :: String -> IO ()
drawFile path = do
    res <- fileToTree path
    case res of
      Left e  -> putStrLn $ "PARSER ERROR:\n" ++ show e
      Right t -> putStrLn $ drawVerticalTree (map showEdge t)
    where
        showEdge (s, Nothing) = s :: String
        showEdge (s, Just i)  = (s :: String) ++ "[" ++ show i ++ "]"
