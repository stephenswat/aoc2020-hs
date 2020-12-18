module Problems.Day17 (solution) where

import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Data.Set (Set, fromList, toList, filter)
import Data.Map (Map, mapKeys, keys, lookup, size, filter, fromList)
import Relude.Extra.Tuple (toSnd)

import Common.Solution (Day)
import Common.World

data Cell
    = Live
    deriving (Eq)

type Coordinate = (Int, Int, Int, Int)

type Automaton = Map Coordinate Cell



-- instance Show Automaton where
--     show _ = " Butt "

instance Tile Cell where
    readTile '#' = Just Live
    readTile _   = Nothing

    showTile Live = '#'

toAutomaton :: World Cell -> Automaton
toAutomaton World{tiles=t} = mapKeys (\(x, y) -> (x, y, 0, 0)) t

neighbours :: Coordinate -> [Coordinate]
neighbours p = map (add p) nbs
    where
        add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1+x2, y1+y2, z1+z2, w1+w2)
        nbs = [(x, y, z, w) | x <- dir, y <- dir, z <- dir, w <- dir]
        dir = [-1, 0, 1]

trans :: Automaton -> Coordinate -> Maybe Cell
trans t p
    | l p == Just Live && n >= 2 && n <= 3 = Just Live
    | n == 3                               = Just Live
    | otherwise                            = Nothing
    where
        l = flip (Data.Map.lookup) t
        n = length . Prelude.filter (== Just Live) . map l . Prelude.filter (/= p) . neighbours $ p

stepAutomaton :: (Set Coordinate -> Set Coordinate) -> Automaton -> Automaton
stepAutomaton f a
    = Data.Map.fromList
    . map (second fromJust)
    . Prelude.filter ((== Just Live) . snd)
    . map (toSnd (trans a))
    . Data.Set.toList
    . f
    . Data.Set.fromList
    . concat
    . map neighbours
    . keys
    $ a

solution :: Day
solution =
    ( q (stepAutomaton (Data.Set.filter (\(_, _, _, w) -> w == 0)))
    , q (stepAutomaton id)
    ) where
        readInput = toAutomaton . parseWorld
        q f = show . size . Data.Map.filter (== Live) . (!! 6) . iterate f . readInput