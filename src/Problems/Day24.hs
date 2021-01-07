module Problems.Day24 (solution) where

import Control.Applicative ((<|>))
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)
import Data.Bifunctor (second)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Map (Map, empty, insertWith, filter, size, keys, lookup, fromList)
import Control.Arrow ((&&&))

import Common.Solution (Day)

data Direction
    = East
    | West
    | SouthEast
    | SouthWest
    | NorthEast
    | NorthWest
    deriving (Show)

data Tile
    = Black
    | White
    deriving (Eq, Show)

type Route = [Direction]

data Hex = Hex Int Int Int deriving (Eq, Ord)

type World = Map Hex Tile

instance Semigroup Hex where
    (Hex x1 y1 z1) <> (Hex x2 y2 z2) = Hex (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid Hex where
    mempty = Hex 0 0 0

dirToCoord :: Direction -> Hex
dirToCoord East      = Hex  1  0 (-1)
dirToCoord West      = Hex (-1)  0  1
dirToCoord SouthEast = Hex  0  1 (-1)
dirToCoord SouthWest = Hex (-1)  1  0
dirToCoord NorthEast = Hex  1 (-1)  0
dirToCoord NorthWest = Hex  0 (-1)  1

flipTile :: Tile -> Tile
flipTile White = Black
flipTile Black = White

parseDirection :: Parser Direction
parseDirection = (e <|> w <|> s <|> n)
    where
        s  = char 's' >> ((char 'w' >> return SouthWest) <|> (char 'e' >> return SouthEast))
        n  = char 'n' >> ((char 'w' >> return NorthWest) <|> (char 'e' >> return NorthEast))
        e  = char 'e' >> return East
        w  = char 'w' >> return West

parseInput :: String -> [Route]
parseInput = map (fromRight (error "Bad parse!") . parse (many1 parseDirection) "") . lines

getWorld :: [Hex] -> World
getWorld = foldl (\a n -> insertWith (\_ o -> flipTile o) n Black a) empty

neighbours :: Hex -> [Hex]
neighbours c = map ((c <>) . dirToCoord) $ ds
    where ds = [East, West, SouthWest, SouthEast, NorthWest, NorthEast]

trans :: World -> Hex -> Maybe Tile
trans t p
    | c == Black && n == 0 || n > 2 = Nothing
    | c == White && n /= 2          = Nothing
    | otherwise                     = Just Black
    where
        l = fromMaybe White . flip (Data.Map.lookup) t
        c = l p
        n = length . Prelude.filter (== Black) . map l . neighbours $ p

stepFloor :: World -> World
stepFloor w
    = fromList
    . map (second fromJust)
    . Prelude.filter (isJust . snd)
    . map (id &&& trans w)
    . concat
    . map neighbours
    . keys
    $ w

solution :: Day
solution =
    ( q id
    , q ((!! 100) . iterate stepFloor)
    ) where q f = show . size . Data.Map.filter (== Black) . f . getWorld . map (mconcat . map dirToCoord) . parseInput