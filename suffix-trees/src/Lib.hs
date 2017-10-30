
import Protolude


module Lib
   ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello World"

data Edge = Edge [String]

data SuffixTree
    = Leaf
    | InnerNode [Edge]

make :: Text -> SuffixTree
make t = undefined

query :: String -> Maybe [Int]
query = undefined
