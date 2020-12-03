module Common where

import Data.Map (Map, fromList, member, lookup)
import Data.Maybe (fromJust, isJust)

type Solution = String -> String

type Day = (Solution, Solution)

data World t = World {
    tiles :: Map (Int, Int) t,
    height :: Int,
    width :: Int
}

data StdTiles
    = Wall
    deriving (Eq)

parseWorld :: (Char -> Maybe t) -> String -> World t
parseWorld f i = World { tiles=fromList tiles, height=h, width=w }
    where
        rows = lines i
        h = length rows
        w = length . head $ rows
        tiles =
            [((x, y), fromJust r)
            | x <- [0..w-1]
            , y <- [0..h-1]
            , let r = f . (!! x) . (!! y) $ rows
            , isJust r]

toStdTile :: Char -> Maybe StdTiles
toStdTile '#' = Just Wall
toStdTile _   = Nothing

getTile :: World t -> (Int, Int) -> Maybe t
getTile w c = Data.Map.lookup c (tiles w)

parseNumbers :: String -> [Integer]
parseNumbers = map read . lines