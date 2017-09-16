{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SnipSh where

import Prelude hiding(id) -- meh

import SnipSh.Config

import Data.Aeson
import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import GHC.Generics

import Network.HTTP.Req

import Control.Monad.Reader
import Control.Monad.Except

data SnipShError = SnipShError
    { reason :: String
    } deriving (Show)

type SnipSh = ReaderT Config (ExceptT SnipShError IO)

instance MonadHttp SnipSh where
    handleHttpException = throwError . SnipShError . show

runSnipSh :: SnipSh a -> IO (Either SnipShError a)
runSnipSh snip = do
    defaultConf <- readDefaultConfig
    case defaultConf of
        Left err -> return $ Left (SnipShError err)
        Right conf -> runExceptT $ runReaderT snip conf

runSnipSh' :: Config -> SnipSh a -> IO (Either SnipShError a)
runSnipSh' c snip = runExceptT $ runReaderT snip c

{- * Config Section -}

defaultConfigFile :: String
defaultConfigFile = "config.json"

readDefaultConfig :: IO (Either String Config)
readDefaultConfig = readConfig defaultConfigFile

getIt :: Int -> SnipSh Snippet
getIt id = do
    c <- ask
    getSnippetById c id

getAll :: SnipSh SnippetIndex
getAll = do
    c <- ask
    getSnippetIndex c

data Snippet = Snippet
    { id :: Int
    , title :: Text
    , file_name :: Text
    , description :: Text
    , author :: Value
    , updated_at :: Text
    , created_at :: Text
    , web_url :: Text
    , raw_url :: Text
    } deriving(Show, Generic)

type SnippetIndex = [Snippet]

instance FromJSON Snippet

getSnippetIndex :: MonadHttp m => Config -> m (SnippetIndex)
getSnippetIndex config = do
    res <- performGetRequest config "snippets"
    return (responseBody res)

getSnippetById :: MonadHttp m => Config -> Int -> m (Snippet)
getSnippetById config id = do
    res <- performGetRequest config $ B.append "snippets/" (BC.pack (show id))
    return (responseBody res)

{- * Low level plumbing -}

performGetRequest :: (FromJSON a, MonadHttp m) =>
                     Config -> B.ByteString -> m (JsonResponse a)
performGetRequest config route =
    let (urlScheme,opts) = mkUrlScheme config route
    in
      req GET urlScheme
      NoReqBody jsonResponse
      $ mappend (mkTokenHeader config) opts

mkUrlScheme config route =
  case parseUrlHttps (B.append (encodeUtf8 $ apiUrl config) route) of
      Just (url,opts)-> (url,opts)
      Nothing -> error "fuck"

mkTokenHeader :: Config -> Option scheme
mkTokenHeader config = header "PRIVATE-TOKEN" $ encodeUtf8 (apiToken config)
