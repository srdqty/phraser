--------------------------------------------------------------------------------
module Phraser.OptionsParser
    ( Options (..)
    , parseOptions
    ) where

--------------------------------------------------------------------------------
import Data.Semigroup ((<>))

--------------------------------------------------------------------------------
import Crypto.Random (Seed, seedFromInteger)
import Options.Applicative
    ( Parser
    , ReadM
    , auto
    , eitherReader
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , infoOption
    , long
    , metavar
    , option
    , optional
    , progDesc
    , short
    , showDefault
    , value
    )

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
data Options = Options
    { numberOfWords :: Int
    , seedValue :: Maybe Seed
    , wordFile :: Maybe FilePath
    }

instance Show Options where
    show (Options n _ _) = "Options{" <> show n <> "}"

--------------------------------------------------------------------------------
parseOptions :: String -> String -> IO Options
parseOptions versionString commitHash
    = execParser
    $ info
      (helper <*> versionOption <*> options)
      (fullDesc <> progDesc description <> header description)
  where
    description = "phraser - Generate a passphrase using the diceware method."
    versionOption = infoOption
                    (versionString <> " " <> commitHash)
                    (long "version" <> short 'v' <> help "Show version")

--------------------------------------------------------------------------------
options :: Parser Options
options = Options
      <$> numberOfWordsOption
      <*> seedValueOption
      <*> wordFileOption

--------------------------------------------------------------------------------
numberOfWordsOption :: Parser Int
numberOfWordsOption = option auto
                    $ long "number-of-words"
                   <> short 'n'
                   <> metavar "NUMBER_OF_WORDS"
                   <> showDefault
                   <> value 10
                   <> help helpString
  where
    helpString = "The number of words to use in the generated passphrase."

--------------------------------------------------------------------------------
seedValueOption :: Parser (Maybe Seed)
seedValueOption = optional
                $ option seedReader
                $ long "seed-integer"
               <> short 's'
               <> metavar "SEED_INTEGER"
               <> help helpString
  where
    helpString = "Optional integer for seeding the random generator."
    seedReader :: ReadM Seed
    seedReader = eitherReader $ \arg -> case reads arg of
        [(n, "")] -> return $ seedFromInteger n
        _         -> Left $ "cannot parse value `" <> arg <> "'"

--------------------------------------------------------------------------------
wordFileOption :: Parser (Maybe FilePath)
wordFileOption = optional
               $ option auto
               $ long "word-file"
              <> short 'w'
              <> metavar "WORD_FILENAME"
              <> help "Optional file for populating the word map."
