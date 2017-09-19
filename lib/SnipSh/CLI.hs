module SnipSh.CLI where

import Prelude hiding(id) -- meh

import SnipSh

import System.IO
import System.Exit
import System.Process
import System.IO.Temp -- withSystemTempFile
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC

import Control.Monad.IO.Class

entry :: IO ()
entry = do
    putStrLn "Welcome to the snipsh!"
    _ <- runSnipSh mainLoop
    putStrLn "Bye." -- unreachable

version = "0.0.1"
prompt = "snipsh v" ++ version ++ " >>="
help =  "Supported Commands are:\n" ++
        "list, get, help, exec exit"

mainLoop :: SnipSh ()
mainLoop = do
    liftIO $ putStrLn prompt'
    liftIO $ putStr ">"
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    dispatch cmd
    mainLoop
  where prompt' = prompt ++ " please enter a command:"

dispatch :: String -> SnipSh ()
dispatch "list" = getAll >>= printIndex
dispatch "get" = do
    liftIO $ putStrLn (prompt ++ " please enter the snippet id:")
    liftIO $ putStr ">"
    liftIO $ hFlush stdout
    idStr <- liftIO getLine
    let id :: Int
        id = read idStr
    getIt id >>= printSnip
    liftIO $ putStrLn "Content:"
    getRaw id >>= (liftIO . putStrLn . BC.unpack)
dispatch "exec" = do
    liftIO $ putStrLn (prompt ++ " please enter the snippet id:")
    liftIO $ putStr ">"
    liftIO $ hFlush stdout
    idStr <- liftIO getLine
    let id :: Int
        id = read idStr
    content <- getRaw id
    editor <- getEditor
    res <- liftIO $ withSystemTempFile ".sh" (\name fh -> do
               hPutStr fh (BC.unpack content)
               hClose fh
               spawnEditor (T.unpack editor) name)
    liftIO $ print res
    _ <- liftIO $ createProcess (shell res)
    return ()
dispatch "exit" = liftIO $ exitSuccess
dispatch _ = liftIO $ putStrLn help

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
