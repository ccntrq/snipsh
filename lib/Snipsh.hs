{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Snipsh where

import Prelude hiding(id) -- meh

import Snipsh.Config

import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Data.Char

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import GHC.Generics

import Network.HTTP.Req
import Data.Proxy

import Control.Monad.Reader
import Control.Monad.Except

import Data.Monoid ((<>))

data SnipshError = SnipshError
    { reason :: String
    } deriving (Show)

type Snipsh = ReaderT Config (ExceptT SnipshError IO)

instance MonadHttp Snipsh where
    handleHttpException = throwError . SnipshError . show

runSnipsh :: Snipsh a -> IO (Either SnipshError a)
runSnipsh snip = do
    defaultConf <- readDefaultConfig
    case defaultConf of
        Left err -> return $ Left (SnipshError err)
        Right conf -> runExceptT $ runReaderT snip conf

runSnipsh' :: Config -> Snipsh a -> IO (Either SnipshError a)
runSnipsh' c snip = runExceptT $ runReaderT snip c

data Snippet = Snippet
    { id :: Int
    , title :: Text
    , file_name :: Text
    , description :: Text
    , author :: Value -- ^ This is a JSON object
    , updated_at :: Text
    , created_at :: Text
    , web_url :: Text
    , raw_url :: Text
    } deriving(Show, Generic)

type SnippetIndex = [Snippet]

instance FromJSON Snippet
instance ToJSON Snippet

getEditor :: Snipsh Text
getEditor = ask >>= (return . editor)

getIndex :: Snipsh SnippetIndex
getIndex = do
    (url,opts) <- prepareRequest "snippets/"
    res <- performJsonGetRequest url opts
    return (responseBody res)

getIt :: Int -> Snipsh Snippet
getIt id = do
    (url,opts) <- prepareRequest (B.append "snippets/" (BC.pack (show id)))
    res <- performJsonGetRequest url opts
    return (responseBody res)

getRaw :: Int -> Snipsh B.ByteString
getRaw id = do
    (url,opts) <- prepareRequest (B.append (B.append "snippets/" (BC.pack (show id))) "/raw")
    res <- performBsGetRequest url opts
    return (responseBody res)

prepareRequest :: B.ByteString -> Snipsh (Url 'Https, Option scheme)
prepareRequest route = do
    accessHeader <- mkTokenHeader
    (urlScheme, urlOpts) <- mkUrlScheme route
    return (urlScheme, urlOpts <> accessHeader)

mkTokenHeader :: Snipsh (Option scheme)
mkTokenHeader = (return . mkHeader) =<< ask
  where
    mkHeader :: Config -> Option scheme
    mkHeader conf = header "PRIVATE-TOKEN" $ encodeUtf8 (apiToken conf)

mkUrlScheme :: B.ByteString -> Snipsh (Url 'Https, Option scheme)
mkUrlScheme route = do
  conf <- ask
  case parseUrlHttps (B.append (encodeUtf8 $ apiUrl conf) route) of
      Just (url,opts)-> return (url,opts)
      Nothing -> throwError $ SnipshError "couldn't parse url"

performJsonGetRequest :: (FromJSON a, MonadHttp m) =>
                         Url scheme -> Option scheme -> m (JsonResponse a)
performJsonGetRequest = performGetRequest jsonResponse

performBsGetRequest :: MonadHttp m =>
                       Url scheme -> Option scheme -> m BsResponse
performBsGetRequest = performGetRequest bsResponse

performGetRequest :: (HttpResponse response, MonadHttp m) =>
                     Proxy response -> Url scheme -> Option scheme -> m response
performGetRequest responseProxy urlScheme opts = req GET urlScheme NoReqBody responseProxy opts



