module Time where

import Data.List

data Time = Time { hour   :: Integer
                 , minute :: Integer
                 , second :: Integer
                } deriving (Eq) 

instance Show Time where
    show = showTime . process

instance Num Time where
    Time h m s + Time h1 m1 s1 = Time (h+h1) (m+m1) (s+s1)
    Time h m s - Time h1 m1 s1 = Time (h-h1) (m-m1) (s-s1)
    Time h m s * Time h1 m1 s1 = Time (h*h1) (m*m1) (s*s1)
    abs = tmap abs
    signum = tmap signum
    fromInteger s = Time 0 0 s

(+:) t s = t + time 0 0 s
(+::) t m = t + time 0 m 0
(+:::) t h = t + time h 0 0

tmap :: (Integer -> Integer) -> Time -> Time
tmap f t = Time (f $ hour t) (f $ minute t) (f $ second t) 

toTuple t = (hour t, minute t, second t) 
fromTuple (h,m,s) = Time h m s

process :: Time -> Time
process = fromTuple . fixHours . fixMinutes . toTuple
    where fixMinutes (h,m,s) = (h, m + s `div` 60, s `mod` 60)
          fixHours   (h,m,s) = (h + m `div` 60, m `mod` 60, s)

showTime :: Time -> String
showTime t = intercalate ":" . map (addZero . ($ t)) $ [hour, minute, second]

addZero :: (Num a, Ord a, Show a) => a -> String
addZero x | x < 10 = '0' : show x
          | otherwise = show x

time = Time

main = do
    print $ time 1 59 61