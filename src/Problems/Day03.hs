module Problems.Day03 (solution) where

import Data.Bifunctor

import Common.Solution (Day)
import Common.World (World, StdTiles(Wall), parseWorld, height, width, getTile, toStdTile)

countTrees :: (Int, Int) -> World StdTiles -> Int
countTrees (dx, dy) m
    = length
    . filter ((== Just Wall) . getTile m . first (`mod` width m))
    . takeWhile ((<= height m) . snd)
    . iterate (bimap (+ dx) (+ dy))
    $ (0, 0)

solution :: Day
solution =
    ( show . countTrees (3, 1) . parseWorld toStdTile
    , show . product . (\m -> [countTrees s m | s <- slopes]) . parseWorld toStdTile
    )
    where slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
