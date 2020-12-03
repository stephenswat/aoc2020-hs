module Problems.Day01 (solution) where

import Common.Solution (Day)
import Common.Parse (parseNumbers)

problemA :: [Integer] -> Integer
problemA i = head [a * b | a <- i, b <- i, a + b == 2020]

problemB :: [Integer] -> Integer
problemB i = head [a * b * c | a <- i, b <- i, c <- i, a + b + c == 2020]

solution :: Day
solution = (show . problemA . parseNumbers, show . problemB . parseNumbers)
