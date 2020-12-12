{-# LANGUAGE TemplateHaskell #-}

module Problems.Day12 (solution) where

import Control.Lens
import Control.Arrow

import Common.Solution (Day)

type Position = (Integer, Integer)

data Command
    = Move Cardinal Integer
    | Forward Integer
    | Turn Rotation
    deriving (Show)

data Cardinal
    = North
    | West
    | South
    | East
    deriving (Show)

data Rotation
    = Port
    | Starboard
    | Aft
    deriving (Show)

data Ship = Ship {
    _pos :: Position,
    _way :: Position,
    _dir :: Cardinal
}

makeLenses ''Ship

initShip :: Ship
initShip = Ship{_pos=(0, 0), _way=(10, 1), _dir=East}

rotate :: Rotation -> Cardinal -> Cardinal
rotate Port      North = West
rotate Port      West  = South
rotate Port      South = East
rotate Port      East  = North
rotate Starboard North = East
rotate Starboard West  = North
rotate Starboard South = West
rotate Starboard East  = South
rotate Aft       North = South
rotate Aft       West  = East
rotate Aft       South = North
rotate Aft       East  = West

parseCommand :: String -> Command
parseCommand ('F':x) = Forward (read x)
parseCommand ('N':x) = Move North (read x)
parseCommand ('W':x) = Move West (read x)
parseCommand ('S':x) = Move South (read x)
parseCommand ('E':x) = Move East (read x)
parseCommand "L90"   = Turn Port
parseCommand "L180"  = Turn Aft
parseCommand "L270"  = Turn Starboard
parseCommand "R90"   = Turn Starboard
parseCommand "R180"  = Turn Aft
parseCommand "R270"  = Turn Port
parseCommand _       = error "Invalid command!"

moveVec :: Cardinal -> Position
moveVec North = ( 0,  1)
moveVec West  = (-1,  0)
moveVec South = ( 0, -1)
moveVec East  = ( 1,  0)

applyMove :: Position -> Integer -> Position -> Position
applyMove (dx, dy) l = (+ l * dx) *** (+ l * dy)

rotateWay :: Rotation -> Position -> Position
rotateWay Port      (x, y) = (-y,  x)
rotateWay Starboard (x, y) = ( y, -x)
rotateWay Aft       (x, y) = (-x, -y)

applyCommandA :: Ship -> Command -> Ship
applyCommandA s (Move d l)  = s & pos %~ applyMove (moveVec d) l
applyCommandA s (Forward l) = s & pos %~ applyMove (moveVec (s ^. dir)) l
applyCommandA s (Turn d)    = s & dir %~ rotate d

applyCommandB :: Ship -> Command -> Ship
applyCommandB s (Move d l)  = s & way %~ applyMove (moveVec d) l
applyCommandB s (Forward l) = s & pos %~ applyMove (s ^. way) l
applyCommandB s (Turn d)    = s & way %~ rotateWay d

solution :: Day
solution =
    ( q applyCommandA
    , q applyCommandB
    ) where q f = show . uncurry (+) . (abs *** abs) . (^. pos) . foldl f initShip . map parseCommand . lines
