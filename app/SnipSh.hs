module SnipSh where

import SnipSh.Config 

-- import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = do
    putStrLn "welcome to the snipSh v0.0.1"
    config <- readDefaultConfig
    print $ config

{- * Config Section -}

defaultConfigFile :: String
defaultConfigFile = "config.json"

readDefaultConfig :: IO (Either String Config)
readDefaultConfig = readConfig defaultConfigFile
