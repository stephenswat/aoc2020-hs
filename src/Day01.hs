module Day01 (solution) where

import Common

problemA :: [Integer] -> Integer
problemA i = head [a * b | a <- i, b <- i, a + b == 2020]

problemB :: [Integer] -> Integer
problemB i = head [a * b * c | a <- i, b <- i, c <- i, a + b + c == 2020]

solution :: (Solution, Solution)
solution = (show . problemA . parseNumbers, show . problemB . parseNumbers)
