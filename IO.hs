module IO where

-- | Print each element of a list on a new line
printLines :: (Show a) => [a] -> IO()
printLines = mapM_ print

-- | Reads from STDIN, returns a list of Strings seperated by '\n'
getLines :: IO [String]
getLines = fmap lines getContents

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

-- | Takes input from STDIN until a blank line is entered.  Returns an IO list
terminal :: IO [String]
terminal = f []
  where f xs = getLine >>= \x -> if x == ""
                                   then return $ reverse xs
                                   else f (x:xs)

-- | Takes input from STDIN until a blank line is entered.  Returns an IO list with the function applied
terminalWith :: Show a => (String -> a) -> IO [a]
terminalWith function = fmap (map function) terminal
