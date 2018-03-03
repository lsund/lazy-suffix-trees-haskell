#!/bin/zsh

stack build --profile
stack exec -- suffix-trees-haskell data/akz/200.txt tree +RTS -p
