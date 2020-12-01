module Common where

type Solution = String -> String

type Day = (Solution, Solution)

parseNumbers :: String -> [Integer]
parseNumbers = map read . lines