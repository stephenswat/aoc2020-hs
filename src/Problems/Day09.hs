module Problems.Day09 (solution) where

import Data.List (inits, tails, nub)
import Data.Set (Set, fromList, member)
import Data.Bifunctor (first)

import Common.Solution (Day)
import Common.Parse (parseNumbers)

sublist :: Integer -> [Integer] -> [Integer]
sublist g x
    | null x = []
    | g <= 0 = []
    | otherwise = c:(sublist (g - c) (tail x)) where c = head x

solveA :: [Integer] -> Integer
solveA
    = snd
    . head
    . filter (not . (uncurry . flip $ member))
    . map (first doubletSums . initLast)
    . filter ((== 26) . length)
    . map (take 26)
    . tails
    where
        doubletSums xs = fromList [x + y | x <- xs, y <- xs, x /= y]
        initLast xs = (init xs, last xs)

solveB :: [Integer] -> Integer
solveB x
    = sumMinMax
    . head
    . filter ((> 1) . length)
    . filter ((== t) . sum)
    . map (sublist t)
    . tails
    $ x
    where
        t = solveA x
        sumMinMax xs = minimum xs + maximum xs

solution :: Day
solution =
    ( show . solveA . parseNumbers
    , show . solveB . parseNumbers
    )
