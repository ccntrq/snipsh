module Main where

import SnipSh

main :: IO ()
main = do
    putStrLn "welcome to the snipSh v0.0.1"
    r <- runSnipSh $ getIt 1
    print r
