{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
module Phraser.Phrase
    ( genPhrase
    , genWord
    , GenError (..)
    ) where

--------------------------------------------------------------------------------
import           Data.Semigroup ((<>))
import           Data.Tuple (swap)

--------------------------------------------------------------------------------
import           Control.Monad.Except (MonadError (..))
import           Control.Monad.State (MonadState (..), get, put, replicateM)
import           Crypto.Random (DRG (..), randomBytesGenerate)
import           Data.ByteArray (Bytes, unpack)
import qualified Data.Map as Map (lookup)
import           Data.Word8 (Word8)

--------------------------------------------------------------------------------
import           Phraser.WordList (wordList)

--------------------------------------------------------------------------------
data GenError = CannotFindWordWithIndex Integer

instance Show GenError where
    show (CannotFindWordWithIndex n) = "Cannot find word with index " <> show n

--------------------------------------------------------------------------------
genPhrase :: (DRG g, MonadError GenError m, MonadState g m) => Int -> m String
genPhrase n = unwords <$> replicateM n genWord

--------------------------------------------------------------------------------
genWord :: (DRG g, MonadError GenError m, MonadState g m) => m String
genWord = do
    (n, g) <- fmap genIntegral get
    put g
    wordLookup n

--------------------------------------------------------------------------------
genIntegral :: (DRG g, Integral n) => g -> (n, g)
genIntegral = swap . fmap fiveBytesToIntegral . swap . randomBytesGenerate 5

--------------------------------------------------------------------------------
fiveBytesToIntegral :: Integral n => Bytes -> n
fiveBytesToIntegral = fiveDiceToIntegral . fmap byteToIntegral . unpack

--------------------------------------------------------------------------------
fiveDiceToIntegral :: Integral n => [n] -> n
fiveDiceToIntegral ns = sum (zipWith (*) ns [1, 10, 100, 1000, 10000])

--------------------------------------------------------------------------------
byteToIntegral :: Integral n => Word8 -> n
byteToIntegral = (+1) . round . (*5) . (/255) . toRational

--------------------------------------------------------------------------------
wordLookup :: (MonadError GenError m) => Integer -> m String
wordLookup n = maybe
               (throwError $ CannotFindWordWithIndex n)
               return
               (Map.lookup n wordList)
