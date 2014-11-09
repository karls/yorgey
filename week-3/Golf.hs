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
histogram :: [Integer] -> String
histogram xs = concat $ intersperse "\n" $ map concat ls
  where
    fs = frequencies xs
    m  = maximum (map snd fs)
    ls = transpose [column y m (lookup y fs) | y <- [0..9]]

frequencies :: [Integer] -> [(Integer, Integer)]
frequencies = elemAndCount . groupBy (==) . sort
  where
    elemAndCount = map (\l -> (head l, (fromIntegral . length) l))

column :: Integer -> Integer -> Maybe Integer -> [String]
column n m Nothing  =
  reverse ([show n] ++ ["="] ++ [" " | _ <- [1..m]])
column n m (Just x) =
  reverse ([show n] ++ ["="] ++ ["*" | _ <- [1..x]] ++ [" " | _ <- [1..(m-x)]])
