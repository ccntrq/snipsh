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
import Data.Proxy

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

getEditor :: SnipSh Text
getEditor = ask >>= (return . editor)

getIt :: Int -> SnipSh Snippet
getIt id = do
    c <- ask
    liftIO $ putStrLn "fetching snippet. this might take a while..."
    getSnippetById c id

getRaw :: Int -> SnipSh B.ByteString
getRaw id = do
    c <- ask
    liftIO $ putStrLn "fetching snippet raw content. this might take a while..."
    getSnippetRaw c id

getAll :: SnipSh SnippetIndex
getAll = do
    c <- ask
    liftIO $ putStrLn "fetching snippet index. this might take a while..."
    getSnippetIndex c

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

getSnippetIndex :: MonadHttp m => Config -> m (SnippetIndex)
getSnippetIndex config = do
    res <- performJsonGetRequest config "snippets"
    return (responseBody res)

getSnippetById :: MonadHttp m => Config -> Int -> m (Snippet)
getSnippetById config id = do
    res <- performJsonGetRequest config $ B.append "snippets/" (BC.pack (show id))
    return (responseBody res)

getSnippetRaw :: MonadHttp m => Config -> Int -> m (B.ByteString)
getSnippetRaw config id = do
    res <- performBsGetRequest config $ B.append (B.append "snippets/" (BC.pack (show id))) "/raw" -- urgh
    return (responseBody res)

{- * Low level plumbing -}

performJsonGetRequest :: (FromJSON a, MonadHttp m) =>
                         Config -> B.ByteString -> m (JsonResponse a)
performJsonGetRequest = performGetRequest jsonResponse

performBsGetRequest :: MonadHttp m =>
                       Config -> B.ByteString -> m (BsResponse)
performBsGetRequest = performGetRequest bsResponse

performGetRequest :: (HttpResponse response, MonadHttp m) =>
                     Proxy response -> Config -> B.ByteString -> m response
performGetRequest responseProxy config route =
    let (urlScheme,opts) = mkUrlScheme config route
    in
      req GET urlScheme
      NoReqBody responseProxy
      $ mappend (mkTokenHeader config) opts

mkUrlScheme config route =
  case parseUrlHttps (B.append (encodeUtf8 $ apiUrl config) route) of
      Just (url,opts)-> (url,opts)
      Nothing -> error "fuck"

mkTokenHeader :: Config -> Option scheme
mkTokenHeader config = header "PRIVATE-TOKEN" $ encodeUtf8 (apiToken config)
