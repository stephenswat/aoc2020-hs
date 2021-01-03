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

type HexCoord = (Int, Int, Int)

type World = Map HexCoord Tile

dirToCoord :: Direction -> HexCoord
dirToCoord East      = ( 1,  0, -1)
dirToCoord West      = (-1,  0,  1)
dirToCoord SouthEast = ( 0,  1, -1)
dirToCoord SouthWest = (-1,  1,  0)
dirToCoord NorthEast = ( 1, -1,  0)
dirToCoord NorthWest = ( 0, -1,  1)

flipTile :: Tile -> Tile
flipTile White = Black
flipTile Black = White

addHex :: HexCoord -> HexCoord -> HexCoord
addHex (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

parseDirection :: Parser Direction
parseDirection = (e <|> w <|> s <|> n)
    where
        s  = char 's' >> ((char 'w' >> return SouthWest) <|> (char 'e' >> return SouthEast))
        n  = char 'n' >> ((char 'w' >> return NorthWest) <|> (char 'e' >> return NorthEast))
        e  = char 'e' >> return East
        w  = char 'w' >> return West

flattenRoute :: Route -> HexCoord
flattenRoute = foldl (\a n -> addHex a (dirToCoord n)) (0, 0, 0)

parseInput :: String -> [Route]
parseInput = map (fromRight (error "Bad parse!") . parse (many1 parseDirection) "") . lines

getWorld :: [HexCoord] -> World
getWorld = foldl (\a n -> insertWith (\_ o -> flipTile o) n Black a) empty

neighbours :: HexCoord -> [HexCoord]
neighbours c = map (addHex c . dirToCoord) $ ds
    where ds = [East, West, SouthWest, SouthEast, NorthWest, NorthEast]

trans :: World -> HexCoord -> Maybe Tile
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
    ) where q f = show . size . Data.Map.filter (== Black) . f . getWorld . map flattenRoute . parseInput