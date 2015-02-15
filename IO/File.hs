module IO.File where

import System.IO
import Data.List.Split

-- | Reads from a file
-- Returns single String of contents    -}
open :: FilePath -> IO String
open file = openFile file ReadMode >>= hGetContents

-- | Reads from a file line by line
-- Returns list of Strings
getLines :: FilePath -> IO [String]
getLines file = open file >>= return . lines

getInts :: FilePath -> IO [Int]
getInts file = open file >>= return . map read . words

getDoubles :: FilePath -> IO [Double]
getDoubles file = open file >>= return . map read . words

readCSV :: FilePath -> IO [[String]]
readCSV file = open file >>= return . map (splitOn ",") . lines

getMultiNums :: (Num a, Read a) => FilePath -> IO [[a]]
getMultiNums file = open file >>= return . map (map read . words) . lines
