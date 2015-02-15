{-#LANGUAGE OverloadedStrings #-}
module IO.Text where

import Data.Text hiding (map)
import Data.Text.IO
import Prelude hiding (lines, words, putStrLn, getContents, getLine, readFile)

-- | Reads from a file
-- Returns single Text of contents
open :: FilePath -> IO Text
open = readFile

-- | Reads from a file line by line
-- Returns list of Strings
getLines :: FilePath -> IO [Text]
getLines file = open file >>= return . lines

printLines :: [Text] -> IO()
printLines = mapM_ putStrLn

readCSV :: FilePath -> IO [[Text]]
readCSV file = open file >>= return . map (splitOn ",") . lines

readLines :: IO [Text]
readLines = getContents >>= return . lines

hackerrankLn :: IO Text
hackerrankLn = getLine >> getLine >>= return

hackerrankList :: IO [Text]
hackerrankList = getLine >> readLines

hackerrank :: IO a -> IO a
hackerrank = (>>) getLine

terminal :: IO [Text]
terminal = f []
  where f xs = getLine >>= \x -> if x == ""
  	                               then return xs
  	                               else f (x:xs)
