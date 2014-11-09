module Golf where

import Data.List

-- |Ex1 - Hopscotch
-------------------
skips :: [a] -> [[a]]
skips as = map (\x -> map (as !!) [x-1,2*x-1..ll-1]) [1..ll]
  where
    ll = length as


-- |Ex2 - Local maxima
----------------------
localMaxima :: [Integer] -> [Integer]
localMaxima ns = map maxima $ filter isMaxima (partitions ns)

maxima :: [Integer] -> Integer
maxima = (!! 1)

isMaxima :: [Integer] -> Bool
isMaxima (x:y:z:[]) = x < y && y > z
isMaxima _          = False

partitions :: [Integer] -> [[Integer]]
partitions xs = map (\x -> (drop (x - 3) . take x) xs) [3..(length xs)]


-- |Ex3 - Histogram
-------------------
frequencies :: [Integer] -> [(Integer, Int)]
frequencies xs = map (\l@(x:_) -> (x, length l)) $ groupBy (\x y -> x == y) $ sort xs

produceColumn :: Int -> Maybe Int -> [String]
produceColumn m Nothing = [" " | _ <- [1..m]]
produceColumn m (Just x) = reverse (["*" | _ <- [1..x]] ++ [" " | _ <- [1..(m-x)]])

histogram :: [Integer] -> String
histogram xs = concat $ intersperse "\n" $ map concat ls
  where
    freqs = frequencies xs
    m = snd $ maximumBy (\(_,x2) (_,y2) -> x2 `compare` y2) freqs
    ls = transpose [(produceColumn m (lookup y freqs)) ++ ["=",(show y)] | y <- [0..9]]
