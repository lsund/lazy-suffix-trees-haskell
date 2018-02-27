
module Draw where

import           Prelude                       (String)

import           Data.Tree
import           Data.Tree.Pretty
import           Protolude

import           Algorithm.Common
import           Algorithm.LazyTree.Functional
import           Data.SuffixTree

import           Reader

-------------------------------------------------------------------------
-- Draw


drawST :: (Tree String -> String) -> STree Char -> IO ()
drawST drawFun = putStr . drawFun . map edgeLabel . toTree
    where
        edgeLabel (Label s l) = take l s

draw :: STree Char -> IO ()
draw = drawST drawTree

drawPretty :: STree Char -> IO ()
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
