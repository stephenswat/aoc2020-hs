module Problems.Day11 (solution) where

import Data.Map (mapWithKey, filter, size, lookup)
import Data.Maybe (catMaybes, listToMaybe)

import Common.Solution (Day)
import Common.World

data PlaneTile
    = Empty
    | Occupied
    deriving (Eq)

type TransFun = PlaneTile -> [PlaneTile] -> PlaneTile

instance Tile PlaneTile where
    readTile 'L' = Just Empty
    readTile '#' = Just Occupied
    readTile _   = Nothing

    showTile Empty = 'L'
    showTile Occupied = '#'

visible :: Int -> (Int, Int) -> [[(Int, Int)]]
visible r (x, y) =
    [[(x + n * dx, y + n * dy) | n <- [1..r]]
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , not (dx == 0 && dy == 0)
    ]

neighbours :: Int -> World PlaneTile -> (Int, Int) -> [PlaneTile]
neighbours r w
    = catMaybes
    . map (listToMaybe . catMaybes . map (\x -> Data.Map.lookup x (tiles w)))
    . visible r

transitionA :: TransFun
transitionA c b
    | c == Empty && n == 0    = Occupied
    | c == Occupied && n <= 3 = Occupied
    | otherwise               = Empty
    where n = length . Prelude.filter (== Occupied) $ b

transitionB :: TransFun
transitionB c b
    | c == Empty && n == 0    = Occupied
    | c == Occupied && n <= 4 = Occupied
    | otherwise               = Empty
    where n = length . Prelude.filter (== Occupied) $ b

stepWorld :: TransFun -> Int -> World PlaneTile -> World PlaneTile
stepWorld trans r w@World{tiles=t} = w{tiles=mapWithKey f t}
    where f p c = trans c (neighbours r w p)

untilRepeat :: Eq a => (a -> a) -> a -> [a]
untilRepeat f s = go Nothing s
    where
        go Nothing b = go (Just b) (f b)
        go (Just a) b
            | a == b = [a]
            | otherwise = a:(go (Just b) (f b))

countOccupied :: World PlaneTile -> Integer
countOccupied = toInteger . size . Data.Map.filter (== Occupied) . tiles

solution :: Day
solution =
    ( show . countOccupied . last . untilRepeat (stepWorld transitionA 1) . parse
    , show . countOccupied . last . untilRepeat (stepWorld transitionB 100) . parse
    ) where parse = parseWorld :: String -> World PlaneTile
