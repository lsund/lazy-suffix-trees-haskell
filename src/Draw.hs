
module Draw where

import Prelude          (String)

import Protolude
import Data.Tree.Pretty
import Data.Tree

import LazyTree.Functional
import Reader

toTree :: STree Char -> Tree (Label Char)
toTree = unfoldTree unfoldEdge . rootEdge
    where
        rootEdge st = (("r", 1 :: Int), st)

-------------------------------------------------------------------------
-- Draw


drawST :: (Tree String -> String) -> STree Char -> IO ()
drawST drawFun = putStr . drawFun . map edgeLabel . toTree
    where
        edgeLabel (s, l) = take l s

draw :: STree Char -> IO ()
draw = drawST drawTree

drawPretty :: STree Char -> IO ()
drawPretty = drawST drawVerticalTree

drawFile :: String -> IO ()
drawFile path = do
    res <- fileToTree path
    case res of
      Left e -> putStrLn $ "PARSER ERROR:\n" ++ show e
      Right t -> putStrLn $ drawVerticalTree (map showEdge t)
    where
        showEdge (s, Nothing) = s :: String
        showEdge (s, Just i) = (s :: String) ++ "[" ++ show i ++ "]"
