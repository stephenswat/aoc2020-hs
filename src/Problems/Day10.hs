module Problems.Day10 (solution) where

import Data.List (sort)
import Data.Bifunctor (bimap)
import Data.Map (Map, empty, insert, lookup)
import Control.Arrow ((&&&))
import Control.Monad.State

import Common.Solution (Day)
import Common.Parse (parseNumbers)

pairwise :: [a] -> [(a, a)]
pairwise (a:b:xs) = (a, b):(pairwise (b:xs))
pairwise _ = []

addStartEnd :: [Integer] -> [Integer]
addStartEnd x = 0:(maximum x + 3):x

solveA :: [Integer] -> Integer
solveA
    = toInteger
    . uncurry (*)
    . bimap length length
    . (filter (== 1) &&& filter (== 3))
    . map (uncurry . flip $ (-))
    . pairwise
    . sort
    . addStartEnd

solveB' :: [Integer] -> Integer -> State (Map Integer Integer) Integer
solveB' xs n = do
    m <- get;
    case Data.Map.lookup n m of
        Just v -> return v;
        Nothing -> case [x | x <- xs, x > n, x <= n + 3] of
            [] -> do
                modify (insert n 1);
                return 1;
            q -> do
                l <- sequence [solveB' xs i | i <- q];
                let p = (sum l);
                modify (insert n p);
                return p;

solveB :: [Integer] -> Integer
solveB
    = (\x -> evalState (solveB' x 0) empty)
    . sort
    . addStartEnd

solution :: Day
solution =
    ( show . solveA . parseNumbers
    , show . solveB . parseNumbers
    )
