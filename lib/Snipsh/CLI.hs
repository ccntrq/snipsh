{-# LANGUAGE DeriveDataTypeable #-}
module Snipsh.CLI where

import Prelude hiding(id) -- meh

import Snipsh

import System.IO
import System.Exit
import System.Process
import System.IO.Temp
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC

import Control.Monad.IO.Class

import System.Console.CmdArgs

entry :: IO ()
entry = do
    cmd <- cmdArgs (modes [list,get,exec] &= help "A convenient CLI util for the GitLab snippet API" &= program "snipsh" &= summary prompt)
    _ <- runSnipsh $ dispatch cmd
    return ()

dispatch :: Command -> Snipsh ()
dispatch List = listSnips
dispatch (Get sID) = getSnip sID
dispatch (Exec sID) = execSnip sID

data Command
    = List
    | Get { snip :: Int }
    | Exec { snip :: Int }
    deriving (Show, Data, Typeable)

-- i find this weird. shouldn't both be argPos 0?
-- they are used in different modes...
getFlags = def &= argPos 0
execFlags = def &= argPos 1

list = List &= help "list all snippets"
get = Get {snip = getFlags} &= help "get a snippet"
exec = Exec {snip = execFlags} &= help "execute a snippet"
version = "0.0.1"
prompt = "snipsh v" ++ version ++ " =<<"

listSnips = getIndex >>= printIndex
getSnip snipId = do
    getIt snipId >>= printSnip
    liftIO $ putStrLn "Content:"
    getRaw snipId >>= (liftIO . putStrLn . BC.unpack)

execSnip snipId = do
    content <- getRaw snipId
    editor <- getEditor
    res <- liftIO $ withSystemTempFile ".sh" (\name fh -> do
               hPutStr fh (BC.unpack content)
               hClose fh
               spawnEditor (T.unpack editor) name)
    _ <- liftIO $ createProcess (shell res)
    return ()

spawnEditor :: String -> String -> IO String
spawnEditor e file = do
    pid <- spawnProcess e [file]
    waitForProcess pid
    text <- readFile file
    return text

printIndex index = do
    liftIO (printRow $ applyPaddings getHeaderNames)
    mapM_ (liftIO . printRow) (mkRows index)

printSnip snip = do
    liftIO $ printRow $ applyPaddings getHeaderNames
    liftIO $ printRow $ applyPaddings $ mkRow snip

printRow row  = do
    mapM_ putStr row
    putStrLn ""

mkRows :: [Snippet] -> [[String]]
mkRows = map (applyPaddings . mkRow)

mkRow :: Snippet -> [String]
mkRow snip = map (\x -> let (_,fn) = x in fn snip) printValueLookup

getHeaderNames :: [String]
getHeaderNames = map (\x -> let (name,_) = x in name) printValueLookup

-- | i care for the order. so a simple list it is
printValueLookup :: [(String, (Snippet -> String))]
printValueLookup =
    [ ("id",          (show . id))
    , ("title",       (T.unpack . title))
    , ("description", (T.unpack . description))
    ]

removeNewlines :: String -> String
removeNewlines str = foldl (removeIt) "" str
  where
    removeIt acc '\r' = acc -- remove carriage return
    removeIt acc '\n' = acc ++ " " -- add whitespace instead of newline
    removeIt acc x = acc ++ [x] -- keepIt


applyPaddings :: [String] -> [String]
applyPaddings row = zipWith applyIt paddings row
  where
    applyIt padFn col = padFn (removeNewlines col)

paddings :: [String -> String]
paddings = [(padToLen 8), (padToLen 40), (padToLen 80)]

padToLen :: Int -> String -> String
padToLen n s
    | length s <= n - 1 = s ++ (take (n -length s) (repeat ' '))
    | length s > (n - 1) = take (n-1) s ++ " "
