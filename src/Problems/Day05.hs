module Problems.Day05 (solution) where

import Data.List (sort)
import Data.Set (toList, fromList, difference)
import Data.Maybe (listToMaybe)

import Common.Solution (Day)

readBin :: Char -> Char -> String -> Integer
readBin z o
    = sum
    . map (\(n, d) -> if d == o then 2^n else 0)
    . zip [0..]
    . reverse

parseSeat :: String -> Integer
parseSeat s = (readBin 'F' 'B' rs) * 8 + (readBin 'L' 'R' cs)
    where (rs, cs) = splitAt 7 s

findMissing :: [Integer] -> Maybe Integer
findMissing s = listToMaybe . toList $ difference r q
    where
        r = fromList [minimum s..maximum s]
        q = fromList s

solution :: Day
solution =
    ( show . maximum . map parseSeat . lines
    , show . findMissing . sort . map parseSeat . lines
    )
