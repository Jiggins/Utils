module IO where

import Data.List.Split

printLines :: (Show a) => [a] -> IO()
printLines = mapM_ print

getLines :: IO [String]
getLines = getContents >>= return . lines

getNumbers :: (Num a, Read a) => IO [a]
getNumbers = getContents >>= return . map read . lines

getMultiNums :: (Num a, Read a) => IO [[a]]
getMultiNums = getContents >>= return . map (map read . words) . lines

hackerrankLn :: IO String
hackerrankLn = getLine >> getLine >>= return

hackerrankList :: IO [String]
hackerrankList = getLine >> getLines

hackerrankListNum :: (Num a, Read a) => IO [a]
hackerrankListNum = getLine >> getNumbers

hackerrank :: IO a -> IO a
hackerrank = (>>) getLine

terminal :: IO [String]
terminal = f []
  where f xs = getLine >>= \x -> if x == ""
                                   then return xs
                                   else f (x:xs)

terminalWith :: Show a => (String -> a) -> IO ()
terminalWith function = terminal >>= mapM_ print . map function
