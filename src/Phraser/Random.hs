{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
module Phraser.Random
    ( newDRG
    ) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import Crypto.Random
    ( ChaChaDRG
    , MonadRandom
    , Seed
    , drgNew
    , drgNewSeed
    )

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
newDRG :: MonadRandom m => Maybe Seed -> m ChaChaDRG
newDRG (Just seed) = return (drgNewSeed seed)
newDRG Nothing = drgNew
