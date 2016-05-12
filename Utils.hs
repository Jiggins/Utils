module Utils where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import System.CPUTime
import Text.Printf
--import Text.Regex (mkRegex, subRegex)

type Decimal = Integer
type Binary  = Integer

{- * Basic Functions -}

-- | Function composition composition.  Composes a one arity function with a
-- two arity.
-- Often called the 'titty operator'.
(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.) . (.)

infixr 9 .:

apply :: (a -> b) -> (a, a) -> (b, b)
apply f (a, b) = (f a, f b)

--------------------------------------------------------------------------------

{- * Numbers -}

chose :: Integral a => a -> a -> a
chose n r = factorial n `div` (factorial r * factorial (n - r))

digitsSum :: Integral a => a -> a
digitsSum 0 = 0
digitsSum x = r + digitsSum q
  where (q,r) = quotRem x 10

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

goldbachConjecture :: Integer -> (Integer, Integer)
goldbachConjecture x = (x - head con, head con)
  where con = filter isPrime . map (x -) . takeWhile (< x) $ tail primes

numLength :: Double -> Integer
numLength = floor . (1 +) . logBase 10

roots :: Floating a => a -> a -> a -> (a, a)
roots a b c = (root (+) a b c, root (-) a b c)
  where root (±) a b c = -b ± sqrt (b^2 - 4*a*c) / 2*a

squareRoot :: Integral a => a -> a
squareRoot = floor . sqrt . fromIntegral

--------------------------------------------------------------------------------

{- * Base Conversions -}

binary :: Decimal -> Binary
binary = toBase 2

toBase :: Decimal -> Integer -> Integer
toBase = undigits .: toBaseList

binaryList :: Decimal -> [Binary]
binaryList = toBaseList 2

binaryZeros :: Binary -> [Binary]
binaryZeros n = replicate r 0 ++ toBaseList 2 n
    where r = 4 - length (toBaseList 2 n) `mod` 5

toDec :: Binary -> Decimal
toDec = undigitsBase 10 . digits

toBaseList :: Integral a => a -> a -> [a]
toBaseList 0 _    = undefined
toBaseList base n = reverse $ baseList n
    where baseList 0 = []
          baseList x = r : baseList q
            where (q,r) = quotRem x base

--------------------------------------------------------------------------------

{- * Sequences -}

binomial :: Integral a => a -> [a]
binomial n = map (n `chose`) [0..n]

collatz :: Integral a => a -> [a]
collatz 0 = [0]
collatz 1 = [1]
collatz x
    | even x = x : collatz (x `div` 2)
    | odd x  = x : collatz ((x * 3) + 1)

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

palindromesBelow :: (Integral b, Num a) => b -> a
palindromesBelow n
  | even n = 2 * (10 ^ (n `div` 2) - 1)
  | odd  n = 11 * (10 ^ ((n - 1) `div` 2)) - 2

triangleNumbers :: [Integer]
triangleNumbers = zipWith ((`div` 2) .: (*)) [1..] [2..]

--------------------------------------------------------------------------------

{- * Strings -}

letterFrequency :: String -> [(Char,Int)]
letterFrequency xs = [(x,c) | x <- ['.'..'z'],
    let c = length $ filter (x ==) xs, c > 0]

readMany :: Read a => String -> [a]
readMany = unfoldr $ listToMaybe . concatMap reads . tails

splitOnLines :: String -> String
splitOnLines = unlines . map (unlines . lines) . splitOn "\n\n"

-- trim :: String -> String
-- trim str = dropWhile (==' ') $ subRegex (mkRegex "[ |,]+$") str ""

--------------------------------------------------------------------------------

{- * Lists - Numbers -}

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / genericLength xs

digits :: Integer -> [Integer]
digits = toBaseList 10

undigits :: Integral a => [a] -> a
undigits = foldl' ((+) . (10 *)) 0

undigitsBase :: Integral a => a -> [a] -> a
undigitsBase n = foldl' (\a b -> a * n + b) 0

--------------------------------------------------------------------------------

{- * Lists -}

butLast ::  Int -> [a] -> [a]
butLast = (zipWith const <*>) . drop

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

-- | https://www.reddit.com/r/haskell/comments/3r75hq/blow_my_mind_in_one_line/cwlkjba
consecutivePairs ::  [a] -> [(a, a)]
consecutivePairs = ap zip tail

-- | O(n^2) - does not need to be Ord - does not sort
frequenciesEq :: Eq a => [a] -> [(a,Int)]
frequenciesEq [] = []
frequenciesEq (x:xs) = (x, 1 + count x xs) : frequenciesEq (remove x xs)

-- | O(nlog(n))
frequencies :: Ord a => [a] -> [(a,Int)]
frequencies = map (head &&& length) . group . sort

isAnagram :: Ord a => [a] -> [a] -> Bool
isAnagram str xs = quicksort str == xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- | https://www.reddit.com/r/haskell/comments/3r75hq/blow_my_mind_in_one_line/cwmm027
lastN ::  Int -> [a] -> [a]
lastN n = foldl' (const . tail) <*> drop n

powerSet :: [a] -> [[a]]
powerSet = filterM (const [True, False])

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
    ++ [x] ++ quicksort [y | y <- xs, y > x]

remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (/=x)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

-- | Does not sort the list but as a drawback it runs in O(n^2).
removeDuplicatesEq :: Eq a => [a] -> [a]
removeDuplicatesEq []     = []
removeDuplicatesEq (x:xs) = rem xs [x]
    where rem [] list     = list
          rem (x:xs) list | x `elem` list = rem xs list
                          | otherwise = rem xs (list ++ [x])

showDuplicates :: Ord a => [a] -> [a]
showDuplicates = concat . filter ((> 1) . length) . group . sort

showDuplicatePairs :: Ord a => [a] -> [(a,Int)]
showDuplicatePairs = map (head &&& length) . filter ((> 1) . length) . group . sort

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from + 1) . drop (from - 1)

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = take n xs : splitInto n (drop n xs)

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

zipWithDef :: (Default a, Default b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDef _ [] []         = []
zipWithDef f (x:xs) []     = f x def : zipWithDef f xs []
zipWithDef f [] (x:xs)     = f def x : zipWithDef f [] xs
zipWithDef f (x:xs) (y:ys) = f x y   : zipWithDef f xs ys

--------------------------------------------------------------------------------

{- * System and IO -}

time :: IO a -> IO a
time f = do
    start <- getCPUTime
    function <- f
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.6f sec\n" (diff :: Double)
    return function

