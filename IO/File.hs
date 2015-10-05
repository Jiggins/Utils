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
getLines = fmap lines . open

getInts :: FilePath -> IO [Int]
getInts = fmap (map read . words) . open

getDoubles :: FilePath -> IO [Double]
getDoubles = fmap (map read . words) . open

readCSV :: FilePath -> IO [[String]]
readCSV = fmap (map (splitOn ",") . lines) . open

getMultiNums :: (Num a, Read a) => FilePath -> IO [[a]]
getMultiNums = fmap (map (map read . words) . lines) . open
