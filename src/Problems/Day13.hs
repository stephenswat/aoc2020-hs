module Problems.Day13 (solution) where

import Control.Arrow ((&&&))
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List (minimumBy)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)

import Common.Solution (Day)

parseInput :: String -> (Integer, [Maybe Integer])
parseInput s = (read m, map go . splitOn "," $ n)
    where
        (m:n:[]) = lines s
        go :: String -> Maybe Integer
        go "x" = Nothing
        go x  = Just (read x)

solveA :: Integer -> [Integer] -> Integer
solveA t
    = uncurry (*)
    . (id &&& ((-t) `mod`))
    . minimumBy (compare `on` ((negate t) `mod`))

solveB :: [Maybe Integer] -> Integer
solveB
    = uncurry (-)
    . ((product . map snd) &&& (fromJust . chineseRemainder))
    . map (second fromJust)
    . filter (isJust . snd)
    . zip [0..]

solution :: Day
solution =
    ( show . uncurry solveA . second catMaybes . parseInput
    , show . solveB . snd . parseInput
    )