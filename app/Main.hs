module Main where

import DicewarePhrase.WordList (wordList)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromJust)

main :: IO ()
main = putStrLn (fromJust $ Map.lookup 11111 wordList)
