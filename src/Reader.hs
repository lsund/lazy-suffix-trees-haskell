module Reader where

import Prelude      (String)

import Protolude
import Text.ParserCombinators.Parsec

import Data.Tree

-- Reads a tree of the following recursive structure
-- [LABEL[CHILDREN ...]]
--
-- for example:
-- [x[a[b[c[]]]b[c[]]c[]]]
-- represents the tree
--
--   x
--   |
-- -----
-- /  |  \
-- a  b  c
-- |  |
-- b  c
-- |
-- c
--
--
-- Stolen from
-- https://stackoverflow.com/questions/26993496/parse-string-to-list-in-haskell
-- data Tree = Tree Char [Tree] deriving Show

symbol :: String -> Parser String
symbol s = string s <* spaces

parseTree :: Parser (Tree Char)
parseTree = do
    character <- noneOf "[]"
    spaces
    subtree <- parseSubTree
    return $ Node character subtree

parseSubTree :: Parser [Tree Char]
parseSubTree = do
    symbol "["
    trees <- sepBy parseTree (symbol ",")
    symbol "]"
    return trees

fileToTree :: FilePath -> IO (Either ParseError (Tree Char))
fileToTree = parseFromFile parseTree
