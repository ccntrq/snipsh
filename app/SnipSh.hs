{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SnipSh where

import SnipSh.Config
import SnipSh.APIClient

import Control.Exception(throwIO)
import Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO

main :: IO ()
main = do
    putStrLn "welcome to the snipSh v0.0.1"
    config <- readDefaultConfig
    print $ config
    case config of
      Right c ->do
          res <- getSnippetIndex c
          print res
          res' <- getSnippetById c 3
          print res'
      Left e -> print e

{- * Config Section -}

defaultConfigFile :: String
defaultConfigFile = "config.json"

readDefaultConfig :: IO (Either String Config)
readDefaultConfig = readConfig defaultConfigFile
