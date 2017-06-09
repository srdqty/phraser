module Main where

import Control.Monad.State (evalStateT, replicateM, StateT, get, put)
import qualified Data.Map as Map (lookup)
import Data.Maybe (fromJust)
import Crypto.Random (getSystemDRG, randomBytesGenerate, SystemDRG)
import Data.ByteArray (unpack, Bytes)

import DicewarePhrase.WordList (wordList)

diceToInt :: Integral a => [a] -> a
diceToInt ns = sum (zipWith (*) ns [1, 10, 100, 1000, 10000])

main :: IO ()
main = getSystemDRG >>= evalStateT genPhrase >>= putStrLn

genPhrase :: StateT SystemDRG IO String
genPhrase = unwords <$> replicateM 10 genWord
    where
        genWord :: StateT SystemDRG IO String
        genWord = do
            drg <- get
            let (ns, drg') = randomBytesGenerate 5 drg :: (Bytes, SystemDRG)
            put drg'
            let n = diceToInt ((+1) . round . (*5) . (/255) . toRational <$> unpack ns)
            return (fromJust $ Map.lookup n wordList)
