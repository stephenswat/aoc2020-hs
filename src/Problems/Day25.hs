module Problems.Day25 (solution) where

import Math.NumberTheory.Powers.Modular (powMod)
import Data.Bifunctor (second)

import Common.Solution (Day)

transform :: Integer -> Integer -> Integer
transform s l = powMod s l 20201227

findLoopSize :: Integer -> Integer
findLoopSize n = head [m | m <- [0..], transform 7 m == n]

readInput :: String -> [(Integer, Integer)]
readInput s = [(k1, k2), (k2, k1)]
    where (k1:k2:_) = map read . lines $ s

solution :: Day
solution =
    ( show . head . map (uncurry transform . second findLoopSize) . readInput
    , \_ -> "Merry Christmas!"
    )