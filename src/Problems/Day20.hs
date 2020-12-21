module Problems.Day20 (solution) where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, second)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, lookup, map, empty, insertWith, keys, elems, filter)
import Data.Maybe (fromMaybe)

import Common.World
import Common.Solution (Day)

data Pixel
    = On
    | Off
    deriving (Eq, Ord)

instance Show Pixel where
    show i = [showTile i]

instance Tile Pixel where
    readTile '#' = Just On
    readTile '.' = Just Off
    readTile _   = Nothing

    showTile On = '#'
    showTile Off = '.'

data SolveState = SolveState {
    sol :: Map (Integer, Integer) Integer
}

count :: Ord a => [a] -> Map a Integer
count i = foldl (\a n -> insertWith (+) n 1 a) empty i

edges :: World Pixel -> [[Pixel]]
edges m = nub $ q ++ (Prelude.map reverse q)
    where
        h = height m
        w = width m
        f c = fromMaybe Off $ Data.Map.lookup c (tiles m)
        q = [
                [f (0  , y  ) | y <- [0..h-1]],
                [f (w-1, y  ) | y <- [0..h-1]],
                [f (x  , 0  ) | x <- [0..w-1]],
                [f (x  , h-1) | x <- [0..w-1]]
            ]

parseTiles :: String -> Map Integer (World Pixel)
parseTiles
    = fromList
    . Prelude.map (\(a:b:_) -> (read . drop 5 $ a, parseWorld b))
    . Prelude.map (splitOn ":\n")
    . splitOn "\n\n"

butt :: Map Integer (World Pixel) -> Map Integer (World Pixel)
butt m
    = Data.Map.filter ((== 4) . length . Prelude.filter (\y -> elem y l) . edges)
    $ m
    where
        l = keys . Data.Map.filter (== 1) . count . concat . Prelude.map edges . elems $ m

findSeed :: Map Integer (World Pixel) -> Integer
findSeed = head . keys . butt

solveA :: Map Integer (World Pixel) -> String
solveA
    = show
    . product
    . keys
    . butt


solution :: Day
solution =
    ( solveA . parseTiles
    , show . findSeed . parseTiles
    )