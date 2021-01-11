{-# OPTIONS_GHC -Wno-orphans #-}

module Problems.Day20 (solution) where

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map (Map, lookup, keys, filter, singleton, mapKeys, insert, fromList)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Set (Set, fromList, filter, map, findMin, delete, null, toList, size, difference)
import Data.Function (on)
import Control.Applicative ((<|>))

import Common.World
import Common.Utils (count)
import Common.Solution (Day)

data Direction
    = North
    | West
    | South
    | East

data Segment = Segment
    { num :: Integer
    , grid :: World Bool
    } deriving (Show)

instance Tile Bool where
    readTile '#' = Just True
    readTile '.' = Just False
    readTile _   = Nothing

    showTile True = '#'
    showTile False = '.'

instance Tile Segment where
    readTile _ = Nothing
    showTile _ = '?'

instance Eq Segment where
    (==) = (==) `on` num

instance Ord Segment where
    compare = compare `on` num

type Edge = [Bool]

type SegMap = Map (Int, Int) Segment

opposite :: Direction -> Direction
opposite North = South
opposite West  = East
opposite South = North
opposite East  = West

edge :: Direction -> World Bool -> Edge
edge d m = case d of
    North -> [f (x  , 0  ) | x <- [0..w-1]]
    West  -> [f (0  , y  ) | y <- [0..h-1]]
    South -> [f (x  , h-1) | x <- [0..w-1]]
    East  -> [f (w-1, y  ) | y <- [0..h-1]]
    where
        h = height m
        w = width m
        f c = fromMaybe False $ Data.Map.lookup c (tiles m)

edges :: World Bool -> [Edge]
edges m = nub $ q ++ (Prelude.map reverse q)
    where q = [edge North m, edge West m, edge South m, edge East m]

parseTiles :: String -> Set Segment
parseTiles
    = Data.Set.fromList
    . Prelude.map (\(a:b:_) -> Segment { num=read . drop 5 $ a, grid=parseWorld b })
    . Prelude.map (splitOn ":\n")
    . splitOn "\n\n"

transW :: World t -> [World t]
transW r0 = [r0, r1, r2, r3, f0, f1, f2, f3]
    where
        r1 = rotate' r0
        r2 = rotate' r1
        r3 = rotate' r2
        f0 = flip' r0
        f1 = flip' r1
        f2 = flip' r2
        f3 = flip' r3

        rotate' World{tiles=t, width=w, height=h} = World{tiles=nt, width=h, height=w}
            where nt = mapKeys (\(x, y) -> (-y + (h - 1), x)) t

        flip' i@World{tiles=t, width=w} = i{tiles=nt}
            where nt = mapKeys (\(x, y) -> (-x + (w - 1), y)) t

transS :: Segment -> [Segment]
transS i@Segment{grid=g} = [i{grid=j} | j <- transW g]

attemptInsert :: SegMap -> (Int, Int) -> Segment -> Maybe (SegMap)
attemptInsert m c@(x, y) s = foldl (<|>) Nothing . fmap go . transS $ s
    where
        fit :: Direction -> Segment -> Bool
        fit d u = maybe True (\t -> ((edge (opposite d)) . grid $ t) == ((edge d) . grid $ u)) $ nb
            where
                nb = case d of
                    North -> Data.Map.lookup (x, y - 1) m
                    West  -> Data.Map.lookup (x - 1, y) m
                    South -> Data.Map.lookup (x, y + 1) m
                    East  -> Data.Map.lookup (x + 1, y) m

        go :: Segment -> Maybe (SegMap)
        go u
            | fit North u && fit West u && fit South u && fit East u = Just (insert c u m)
            | otherwise = Nothing

assemble :: Set Segment -> Maybe (World Segment)
assemble i = case result of
    Just m    -> Just (makeWorld m)
    _         -> Nothing
    where
        seed = findMin . corners $ i
        rest = delete seed i
        result = foldl (<|>) Nothing [assembleHelper True (singleton (0, 0) x) (1, 0) rest | x <- transS seed]

        corners :: Set Segment -> Set Segment
        corners m = Data.Set.filter ((== 4) . length . Prelude.filter (\y -> elem y l) . edges . grid) $ m
            where
                l = keys . Data.Map.filter (== (1 :: Integer)) . count . concat . Data.Set.map (edges . grid) $ m

        makeWorld :: SegMap -> World Segment
        makeWorld m = World
            { tiles=m
            , width=(+1) . maximum . fmap fst . keys $ m
            , height=(+1) . maximum . fmap snd . keys $ m
            }

        assembleHelper :: Bool -> SegMap -> (Int, Int) -> Set Segment -> Maybe (SegMap)
        assembleHelper l g c@(x, y) r
            | Data.Set.null r = Just g
            | Prelude.null cands = if l then assembleHelper False g (0, y + 1) r else Nothing
            | otherwise = (\(g', r') -> assembleHelper True g' (x + 1, y) r') . head $ cands
            where
                cands = [(fromJust q, delete s r) | s <- Data.Set.toList r, let q = attemptInsert g c s, isJust q]

seaMonster :: (Int, Int) -> [(Int, Int)]
seaMonster (x, y) = [
        (x   , y+1), (x+1 , y+2), (x+4 , y+2), (x+5 , y+1), (x+6 , y+1),
        (x+7 , y+2), (x+10, y+2), (x+11, y+1), (x+12, y+1), (x+13, y+2),
        (x+16, y+2), (x+17, y+1), (x+18, y+0), (x+18, y+1), (x+19, y+1)
    ]

flatten :: World Segment -> World Bool
flatten World{tiles=t, width=w, height=h} = World{tiles=Data.Map.fromList nt, width=w*(tw - 2), height=h*(th - 2)}
    where
        tl = fromJust . Data.Map.lookup (0, 0) $ t
        tw = width . grid $ tl
        th = height . grid $ tl
        nt = [
                (
                    (tx * (tw - 2) + ix - 1, ty * (th - 2) + iy - 1),
                    fromMaybe False . (flip getTile) (ix, iy) . grid . fromJust . Data.Map.lookup (tx, ty) $ t
                )
                | tx <- [0..w-1]
                , ty <- [0..h-1]
                , ix <- [1..tw-2]
                , iy <- [1..th-2]
            ]

roughness :: World Bool -> Int
roughness g@World{tiles=f, width=w, height=h} = size (difference allTrues allMonst)
    where
        isMonster c = and . fmap (fromMaybe False . getTile g) . seaMonster $ c
        allTrues = Data.Set.fromList . keys . Data.Map.filter id $ f
        allMonst = Data.Set.fromList [c | x <- [0..w-1], y <- [0..h-1], isMonster (x, y), c <- seaMonster (x, y)]

solveA :: World Segment -> Integer
solveA g@World{width=w, height=h}
    = product
    . fmap (num . fromJust . getTile g)
    $ [(0, 0), (0, h-1), (w-1, 0), (w-1, h-1)]

solveB :: World Segment -> Int
solveB
    = minimum
    . fmap roughness
    . transW
    . flatten

solution :: Day
solution =
    ( show . solveA . fromJust . assemble . parseTiles
    , show . solveB . fromJust . assemble . parseTiles
    )
