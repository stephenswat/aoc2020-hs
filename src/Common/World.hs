module Common.World where

import Data.List (intercalate)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust, isJust)

data World t = World {
    tiles :: Map (Int, Int) t,
    height :: Int,
    width :: Int
} deriving (Eq)

class Tile a where
    readTile :: Char -> Maybe a
    showTile :: a -> Char

data StdTile
    = Wall
    deriving (Eq)

instance Tile StdTile where
    readTile '#' = Just Wall
    readTile _   = Nothing

    showTile Wall = '#'

parseWorld :: Tile t => String -> World t
parseWorld i = World { tiles=fromList t, height=h, width=w }
    where
        rows = lines i
        h = length rows
        w = length . head $ rows
        t = [((x, y), fromJust r)
            | x <- [0..w-1]
            , y <- [0..h-1]
            , let r = readTile . (!! x) . (!! y) $ rows
            , isJust r]

showWorld :: Tile t => World t -> String
showWorld m@World { height=h, width=w }
    = intercalate "\n" [[maybe '.' showTile . getTile m $ (x, y) | x <- [0..w-1]] | y <- [0..h-1]]

getTile :: World t -> (Int, Int) -> Maybe t
getTile w c = Data.Map.lookup c (tiles w)
