{-# LANGUAGE DeriveDataTypeable #-}
module Snipsh.CLI where

import Prelude hiding(id) -- meh

import Snipsh

import System.IO
import System.Process
import System.IO.Temp
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC

import Control.Monad.IO.Class

import System.Console.CmdArgs

-- | Entry point. parses mode and args and dispatch into Snipsh
entry :: IO ()
entry = do
    cmd <- cmdArgs (modes [list,get,exec] &= help "A convenient CLI util for the GitLab snippet API" &= program "snipsh" &= summary prompt)
    _ <- runSnipsh $ dispatch cmd
    return ()

{- * Command line argument handling -}

version :: String
version = "0.0.9"

prompt :: String
prompt = "snipsh v" ++ version ++ " =<<"


-- | Possible modes of the cli
data Command
    = List
    | Get { snipid :: Int }
    | Exec { snipid :: Int }
    deriving (Show, Data, Typeable)

{- * Command Constructors -}

list :: Command
list = List &= help "list all snippets"

get :: Command
get = Get {snipid = snipFlags 0} &= help "get a snippet"

-- | i would expect that in both modes the first argument should be at pos '0'.
exec :: Command
exec = Exec {snipid = snipFlags 1} &= help "execute a snippet"

-- | dispatch into the Snipsh functions based on the given args
dispatch :: Command -> Snipsh ()
dispatch List = listSnips
dispatch (Get sID) = getSnip sID
dispatch (Exec sID) = execSnip sID

-- | Flags for the snip with position argument
snipFlags :: (Default val, Data val) => Int -> val
snipFlags pos = def &= argPos pos

{- * Snipsh CLI functions -}

-- | prints a table of your personal snips
listSnips :: Snipsh ()
listSnips = getIndex >>= printIndex

-- | prints info about a single snippet and its raw content
getSnip :: Int -> Snipsh ()
getSnip snipId = do
    getIt snipId >>= printSnip
    liftIO $ putStrLn "Content:\n\n'"
    getRaw snipId >>= (liftIO . putStrLn . BC.unpack)

-- | this will launch the editor specified in the config with the raw content
--   of the snippet. you can then edit the content of the snippet to your needs
--   when you close the editor the saved contents will be executed.
--   to abort exectuion you can just delete or comment out the whole file.
execSnip :: Int -> Snipsh ()
execSnip snipId = do
    content <- getRaw snipId
    editor <- getEditor
    res <- liftIO $ withSystemTempFile ".sh" (\loc fh -> do
               hPutStrLn fh disclaimer
               hPutStr fh (BC.unpack content)
               hClose fh
               spawnEditor (T.unpack editor) loc)
    _ <- liftIO $ createProcess (shell res)
    return ()
  where
    disclaimer =
        "# " ++ prompt ++ "\n"
        ++ "# Edit the snippet to your needs, save and close to execute.\n"
        ++ "# Delete or comment out all content to abort ececution.\n"

-- | open editor e with file
spawnEditor :: String -> String -> IO String
spawnEditor e file = do
    pid <- spawnProcess e [file]
    _ <- waitForProcess pid -- handle exit code
    text <- readFile file
    return text

{- * pppp - pants pocket pretty printer -}

printIndex :: [Snippet] -> Snipsh ()
printIndex index = do
    liftIO (printRow $ applyPaddings getHeaderNames)
    mapM_ (liftIO . printRow) (mkRows index)

printSnip :: Snippet -> Snipsh ()
printSnip snip = do
    liftIO $ printRow $ applyPaddings getHeaderNames
    liftIO $ printRow $ applyPaddings $ mkRow snip

printRow :: [String] -> IO ()
printRow row  = do
    mapM_ putStr row
    putStrLn ""

mkRows :: [Snippet] -> [[String]]
mkRows = map (applyPaddings . mkRow)

mkRow :: Snippet -> [String]
mkRow snip = map (\x -> let (_,fn) = x in fn snip) printValueLookup

getHeaderNames :: [String]
getHeaderNames = map (\x -> let (n,_) = x in n) printValueLookup

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
padToLen n s =
    if  length s <= (n - 1)
        then s ++ (take (n -length s) (repeat ' '))
        else take (n-1) s ++ " "
