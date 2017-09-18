module SnipSh.CLI where

import Prelude hiding(id) -- meh

import SnipSh

import qualified Data.Text as T

entry :: IO ()
entry = do
    putStrLn "Welcome to the snipSh! v0.0.1"
    snip <- runSnipSh $ getIt 2
    case snip of 
        Left err -> error $ show err
        Right snip -> do 
            printRow $ applyPaddings getHeaderNames
            printRow $ applyPaddings $ mkRow snip
            putStrLn ""
            putStrLn ""
            print snip

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
