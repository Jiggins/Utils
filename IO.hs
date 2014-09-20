module IO where

import System.IO
import System.Environment
import Data.List.Split

-- | Reads from a file
-- Returns single String of contents    -}
open :: FilePath -> IO String
open file = openFile file ReadMode >>= hGetContents >>= return

-- | Reads from a file line by line
-- Returns list of Strings
getLines :: FilePath -> IO [String]
getLines file = open file >>= return . lines

getInts :: FilePath -> IO [Int]
getInts file = open file >>= return . map read . words

getDoubles :: FilePath -> IO [Double]
getDoubles file = open file >>= return . map read . words

printLines :: (Show a) => [a] -> IO()
printLines = mapM_ print

readCSV :: FilePath -> IO [[String]]
readCSV file = open file >>= return . map (splitOn ",") . lines

readLines :: IO [String]
readLines = getContents >>= return . lines

readNumbers :: (Num a, Read a) => IO [a]
readNumbers = getContents >>= return . map read . lines

getMultiNums :: (Num a, Read a) => FilePath -> IO [[a]]
getMultiNums file = open file >>= return . map (map read . words) . lines

readMultiNums :: (Num a, Read a) => IO [[a]]
readMultiNums = getContents >>= return . map (map read . words) . lines

hackerrankLn :: IO String
hackerrankLn = getLine >> getLine >>= return

hackerrankList :: IO [String]
hackerrankList = getLine >> readLines

hackerrankListNum :: (Num a, Read a) => IO [a]
hackerrankListNum = getLine >> readNumbers

hackerrank :: IO a -> IO a
hackerrank = (>>) getLine

terminal :: IO [String]
terminal = f []
  where f xs = getLine >>= \x -> if x == ""
                                   then return xs 
                                   else f (x:xs)

terminalWith :: Show a => (String -> a) -> IO ()
terminalWith function = terminal >>= mapM_ print . map function