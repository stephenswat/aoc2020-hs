module Problems.Day16 (solution) where

import Data.Bifunctor (second)
import Data.Set (fromList)
import Data.Map (Map, fromList, elems, toList)
import Data.List (transpose, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Common.Solver (optionSolver)
import Common.Solution (Day)

type Range = (Integer, Integer)
type Field = [Range]
type Ticket = [Integer]

data Input = Input
    { fields :: Map String Field
    , mytick :: Ticket
    , nbtick :: [Ticket]
    } deriving (Show)

parseInput :: String -> Input
parseInput s = Input
    { fields=Data.Map.fromList . map parseField . lines $ f
    , mytick=parseTicket . last . lines $ m
    , nbtick=map parseTicket . tail . lines $ n
    }
    where
        [f, m, n] = splitOn "\n\n" s
        parseRange i = (read l, read h) where (l:h:_) = splitOn "-" i
        parseField i = (l, map parseRange . splitOn " or " $ r) where (l:r:_) = splitOn ": " i
        parseTicket = map (read :: String -> Integer) . splitOn ","

matchField :: Integer -> Field -> Bool
matchField v = or . map (inRange v)
    where inRange w (l, h) = w >= l && w <= h

discardInvalid :: Input -> Input
discardInvalid i = i{nbtick=[t | t <- (nbtick i), and [matchField v allRngs | v <- t]]}
    where allRngs = concat . elems . fields $ i

solveA :: Input -> Integer
solveA i = sum [x | x <- concat . nbtick $ i, not . matchField x $ concat . elems . fields $ i]

solveB :: Input -> Integer
solveB i
    = product
    . map (((mytick i) !!) . fromInteger . fst)
    . filter (isPrefixOf "departure" . snd)
    . toList
    . fromJust
    . optionSolver
    . Data.Map.fromList
    . map (second (\vs -> Data.Set.fromList [x | (x, y) <- toList . fields $ i, and [matchField v y | v <- vs]]))
    . zip [0..]
    . transpose
    $ (mytick i):(nbtick i)

solution :: Day
solution =
    ( show . solveA . parseInput
    , show . solveB . discardInvalid . parseInput
    )