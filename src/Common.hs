module Common where

type Solution = String -> String

parseNumbers :: String -> [Integer]
parseNumbers = map read . lines