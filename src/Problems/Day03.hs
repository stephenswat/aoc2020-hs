module Problems.Day03 (solution) where

import Data.Set (Set, fromList, member)
import Data.Bifunctor

import Common

data World = World {
    trees :: Set (Int, Int),
    height :: Int,
    width :: Int
}

parseWorld :: String -> World
parseWorld i = World { trees=fromList trees, height=h, width=w }
    where
        rows = lines i
        h = length rows
        w = length . head $ rows
        trees = [(x, y) | x <- [0..w-1], y <- [0..h-1], (rows !! y) !! x == '#']

countTrees :: (Int, Int) -> World -> Int
countTrees (dx, dy) m
    = length
    . filter (isTree m)
    . takeWhile ((<= height m) . snd)
    . iterate (bimap (+ dx) (+ dy))
    $ (0, 0)
    where isTree m (x, y) = member (x `mod` width m, y) $ trees m

solution :: Day
solution =
    ( show . countTrees (3, 1) . parseWorld
    , show . product . (\m -> [countTrees s m | s <- slopes]) . parseWorld
    )
    where slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
