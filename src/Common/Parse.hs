module Common.Parse where

parseNumbers :: String -> [Integer]
parseNumbers = map read . lines
