module Main where

import Data.Char (digitToInt)

-- Excercises 1-4
toDigits    :: Integer -> [Integer]
toDigits 0 = []
toDigits n = map (toInteger . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ double xs
  where double = zipWith ($) (cycle [id, (*2)])

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concat $ map toDigits xs

validate :: Integer -> Bool
validate n = checkSum `mod` 10 == 0
  where checkSum = (sumDigits . doubleEveryOther . toDigitsRev) n


-- Excercises 5-6
type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _         = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
