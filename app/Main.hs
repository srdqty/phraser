{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
module Main where

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Paths_phraser (version)

-------------------------------------------------------------------------------
import Phraser.OptionsParser (Options (..), parseOptions)
import Phraser.Phrase (genPhrase)
import Phraser.Random (newDRG)

-------------------------------------------------------------------------------
main :: IO ()
main = do
    (Options n s _) <- parseOptions (showVersion version) $(gitHash)
    drg <- newDRG s
    errorOrPhrase <- runExceptT $ evalStateT (genPhrase n) drg
    either print putStrLn errorOrPhrase
