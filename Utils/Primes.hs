module Utils.Primes where

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
    where shw []     = []
          shw (x:xs) = concat (tail (sumPowers x) : map sumPowers xs)
          sumPowers x = [" + ", show $ fst x, "^", show $ snd x]

-- | Euler's totient function
totient :: Integer -> Integer
totient = product . map (\(p,m) -> (p - 1) * p ^ (m-1)) . primeFactorPairs

-- | Given `n`, the product of two prime numbers, finds the two prime numbers.
fermatsLittleTheorem :: (Floating r, RealFrac r) => r -> (r, r)
fermatsLittleTheorem n = f . fromIntegral . (+1) . floor $ sqrt n
  where root = sqrt . subtract n . (^2)
        f x  | isInt 7 (root x) = (x - root x, x + root x)
             | otherwise        = f (x + 1)

isInt :: (Integral a, RealFrac r) => a -> r -> Bool
isInt n x = round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0
