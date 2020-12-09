module Problems.Day09 (solution) where

import Data.List (tails)
import Data.Set (fromList, member)
import Data.Bifunctor (first)
import Data.Tuple.Extra ((&&&))

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
    . map (first doubletSums . (init &&& last))
    . filter ((== 26) . length)
    . map (take 26)
    . tails
    where doubletSums xs = fromList [x + y | x <- xs, y <- xs, x /= y]

solveB :: [Integer] -> Integer
solveB x
    = uncurry (+)
    . (minimum &&& maximum)
    . head
    . filter ((> 1) . length)
    . filter ((== t) . sum)
    . map (sublist t)
    . tails
    $ x
    where t = solveA x

solution :: Day
solution =
    ( show . solveA . parseNumbers
    , show . solveB . parseNumbers
    )
