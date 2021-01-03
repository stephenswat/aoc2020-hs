module Problems.Day23 (solution) where

import Data.Char (isDigit)

import Common.Solution (Day)

type GameState = [Int]

splitAtDestination :: Int -> Int -> [Int] -> ([Int], [Int])
splitAtDestination d m x
    | elem e x = (l ++ [head r], tail r)
    | otherwise = splitAtDestination (d - 1) m x
    where
        e = (d `mod` m)
        (l, r) = break (== e) x

stepGame :: GameState -> GameState
stepGame i@(c:n1:n2:n3:x) = l ++ [n1, n2, n3] ++ r
    where
        (l, r) = splitAtDestination (c - 1) (1 + length i) (x ++ [c])
stepGame _ = error "Illegal game state!"

answerA :: GameState -> String
answerA i = concat . map show $ (tail r) ++ l
    where
        (l, r) = break (== 1) i

solution :: Day
solution =
    ( answerA . (!! 100) . iterate stepGame . readInput
    , \_ -> "Not implemented"
    ) where readInput = map (read . return) . filter isDigit