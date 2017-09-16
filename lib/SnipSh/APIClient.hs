{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module SnipSh.APIClient
    ( Snippet(..)
    , SnippetIndex
    , getSnippetIndex
    , getSnippetById
    ) where

import Network.HTTP.Req
import SnipSh.Config
import Data.Text.Encoding
import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Prelude hiding(id) -- meh

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
