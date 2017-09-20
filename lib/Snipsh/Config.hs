{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Snipsh.Config where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString as B

-- | The Config Type
data Config = Config
    { apiToken :: Text
    , apiUrl   :: Text
    , editor    :: Text
    } deriving (Show, Generic)

-- | FromJson Instance for our config is derived by the compiler
instance FromJSON Config

{- |
 - we use 'config.json' as the default location
 - @
 - defaultConfigFile = "config.json"
 - @
 -}
defaultConfigFile :: String
defaultConfigFile = "config.json"

{- |
 - tries to open the config an decode it or report an error.
 - this is used to supply the Snipsh monad with a default config file
 -}
readDefaultConfig :: IO (Either String Config)
readDefaultConfig = readConfig defaultConfigFile

{- |
 - try to read a config from a given location
 -}
readConfig :: String -> IO (Either String Config)
readConfig configFile = eitherDecodeStrict <$> B.readFile configFile
