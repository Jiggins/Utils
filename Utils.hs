module Utils where

import IO
import IO.Text

import Control.Monad
import Data.List
import Data.List.Split
import System.CPUTime
import Text.Printf
--import Text.Regex (mkRegex, subRegex)

type Decimal = Integer
type Binary  = Integer

{- * Basic Functions -}

-- | Function composition composition.  Composes a one arity frnction with a
-- two arity.
-- Often called the 'titty operator'.
(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.) . (.)

apply :: (a -> b) -> (a, a) -> (b, b)
apply f (a, b) = (f a, f b)

--------------------------------------------------------------------------------

{- * Numbers -}

chose :: Integral a => a -> a -> a
chose n r = factorial n `div` factorial r * factorial (n-r)

digitsSum :: Integral a => a -> a
digitsSum 0 = 0
digitsSum x = r + digitsSum q
    where (q,r) = quotRem x 10

factorial :: (Enum a, Num a) => a -> a
factorial n =  product [1..n]

goldbachConjecture :: Integer -> (Integer, Integer)
goldbachConjecture x = (x - head con, head con)
    where con = filter isPrime . map (x-) . takeWhile (<x) $ tail primes

numLength :: Double -> Integer
numLength = floor . (+1) . logBase 10

squareRoot :: Integral a => a -> a
squareRoot = floor . sqrt . fromIntegral

--------------------------------------------------------------------------------

{- * Base Conversions -}

binary :: Decimal -> Binary
binary = toBase 2

binaryList :: Decimal -> [Binary]
binaryList = toBaseList 2

binaryZeros :: Binary -> [Binary]
binaryZeros n = replicate r 0 ++ (toBaseList 2) n
    where r = 4 - length (toBaseList 2 n) `mod` 4

binListToDec :: [Binary] -> Decimal
binListToDec [] = 0
binListToDec (x:xs)
    | x == 1 = 2 ^ length xs  + binListToDec xs
    | x == 0 = binListToDec xs

toDec :: Binary -> Decimal
toDec = binListToDec . digits

toBase :: Decimal -> Integer -> Integer
toBase = undigits .: toBaseList

toBaseList :: Integral a => a -> a -> [a]
toBaseList base 0 = undefined
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

triangleNumbers :: [Integer]
triangleNumbers = zipWith ((`div` 2) .: (*)) [1..] [2..]

--------------------------------------------------------------------------------

{- * Prime numbers and factorisation -}

primes :: [Integer]
primes = 2 : [x | x <- [3,5..],
  and [x `mod` y /= 0 | y <- takeWhile (<= squareRoot x) primes]]

primesBetween :: Integer -> Integer -> [Integer]
primesBetween low high = dropWhile (<= low) $ takeWhile (<= high) primes

primeFactors :: Integer -> [Integer]
primeFactors n | n <= 1 = []
               | otherwise = factor : primeFactors (n `div` factor)
                   where factor = head [x | x <- primes, n `mod` x == 0]

primeFactorPairs :: Integer -> [(Integer, Int)]
primeFactorPairs = frequencies . primeFactors

isCoPrime :: (Integral a) => a -> a -> Bool
isCoPrime x y = gcd x y == 1

isPrime :: Integer -> Bool
isPrime n | n > 1 = primeFactors n == [n]
          | otherwise = False

factors :: Integer -> [Integer]
factors = removeDuplicates . map product . powerSet . primeFactors

factorPairs :: Integer -> [(Integer, Integer)]
factorPairs n = zip (factors n) (reverse $ factors n)

numberOfFactors :: Integer -> Int
numberOfFactors = product . map (\x -> 1 + snd x) . primeFactorPairs

showPrimeFactors :: Integer -> String
showPrimeFactors = concat . shw . primeFactorPairs
    where shw (x:xs) = concat (tail (sumPowers x) : map sumPowers xs)
          sumPowers x = [" + ", show $ fst x, "^", show $ snd x]

-- | Euler's totient function
totient :: Integer -> Integer
totient = product . map (\(p,m) -> (p - 1) * p ^ (m-1)) . primeFactorPairs

--------------------------------------------------------------------------------

{- * Strings -}

letterFrequency :: String -> [(Char,Int)]
letterFrequency xs = [(x,c) | x <- ['.'..'z'],
    let c = length $ filter (x ==) xs, c > 0]

splitOnLines :: String -> String
splitOnLines = unlines . map (unlines . lines) . splitOn "\n\n"

--trim :: String -> String
--trim str = dropWhile (==' ') $ subRegex (mkRegex "[ |,]+$") str ""

--------------------------------------------------------------------------------

{- * Lists - Numbers -}

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / genericLength xs

digits :: Integer -> [Integer]
digits = toBaseList 10

undigits :: Integral a => [a] -> a
undigits = foldl' (\a b -> a * 10 + b) 0

--------------------------------------------------------------------------------

{- * Lists -}

count :: (Eq a) => a -> [a] -> Int
count x xs = length [y | y <- xs, y == x]

-- | O(n^2) - does not need to be Ord - does not sort
frequenciesEq :: Eq a => [a] -> [(a,Int)]
frequenciesEq [] = []
frequenciesEq (x:xs) = (x, 1 + count x xs) : frequenciesEq (remove x xs)

-- | O(nlog(n))
frequencies :: Ord a => [a] -> [(a,Int)]
frequencies = map (\x -> (head x, length x)) . group . sort

isAnagram :: Ord a => [a] -> [a] -> Bool
isAnagram str xs = quicksort str == xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

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
removeDuplicatesEq (x:xs) = rem xs [x]
    where rem [] list     = list
          rem (x:xs) list | x `elem` list = rem xs list
                          | otherwise = rem xs (list ++ [x])

showDuplicates :: Ord a => [a] -> [a]
showDuplicates = concat . filter (\x -> length x > 1) . group . sort

showDuplicatePairs :: Ord a => [a] -> [(a,Int)]
showDuplicatePairs = map (\x -> (head x, length x))
    . filter (\x -> length x > 1) . group . sort

slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from + 1) . drop (from - 1)

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = take n xs : (splitInto n $ drop n xs)

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

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