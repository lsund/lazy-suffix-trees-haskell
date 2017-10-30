
module Lib
   ( someFunc
    ) where

import Protolude

someFunc :: IO ()
someFunc = putStrLn "Hello World"

data Edge = Edge [Text]

data SuffixTree
    = Leaf
    | InnerNode [Edge]

make :: Text -> SuffixTree
make t = undefined

query :: Text -> Maybe [Int]
query = undefined
