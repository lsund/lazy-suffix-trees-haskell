#!/bin/zsh

stack build --profile
stack exec -- suffix-trees-haskell +RTS -p
