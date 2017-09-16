{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module SnipSh.Config 
    ( Config(..)
    , readConfig
    ) where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy as B

{- * Config Section -}

-- | The Config Type
data Config = Config
    { api_token :: Text
    , api_url   :: Text
    , editor    :: Text
    } deriving (Show, Generic)

instance FromJSON Config
-- instance ToJSON Config where
--     toEncoding = genericToEncoding defaultOptions

-- | tries to open the config an decode it or report an error
readConfig :: String -> IO (Either String Config)
readConfig configFile = eitherDecode <$> B.readFile configFile