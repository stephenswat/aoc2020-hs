module Problems.Day03 (solution) where

import Data.Set

import Common

data Map = Map {
    trees :: Set (Int, Int),
    height :: Int,
    width :: Int
}

parseMap :: String -> Map
parseMap i = Map { trees=fromList trees, height=h, width=w }
    where
        rows = lines i
        h = length rows
        w = length . head $ rows
        trees = [(x, y) | x <- [0..w - 1], y <- [0..h - 1], (rows !! y) !! x == '#']

countSlope :: (Int, Int) -> (Int, Int) -> Map -> Int
countSlope c@(x, y) d@(dx, dy) m
    | y >= height m = 0
    | isTree c m    = 1 + next
    | otherwise     = next
    where
        isTree (x, y) m = member (x `mod` width m, y) $ trees m
        next = countSlope (x + dx, y + dy) d m

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solution :: Day
solution =
    ( show . countSlope (0, 0) (3, 1) . parseMap
    , show . product . (\m -> [countSlope (0, 0) s m | s <- slopes]) . parseMap
    )
