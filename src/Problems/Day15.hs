{-# LANGUAGE TemplateHaskell #-}

module Problems.Day15 (solution) where

import Control.Lens
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, lookup, insert)

import Common.Solution (Day)

data State = State
    { _occ :: Map Integer Integer
    , _lst :: Integer
    , _idx :: Integer
    }

makeLenses ''State

updateState :: State -> State
updateState s
    = (lst %~ maybe 0 ((s ^. idx) -) . (flip Data.Map.lookup) (s ^. occ))
    . (occ %~ insert (s ^. lst) (s ^. idx))
    . (idx +~ 1)
    $ s

initState :: [Integer] -> State
initState n = State
    { _occ=fromList (zip (init n) [0..])
    , _lst=last n
    , _idx=toInteger . length . init $ n
    }

solve :: Integer -> State -> Integer
solve i s
    | (s ^. idx) >= i - 1 = (s ^. lst)
    | otherwise = solve i . updateState $ s

solution :: Day
solution =
    ( f 2020
    , f 30000000
    ) where f n = show . solve n . initState . map read . splitOn ","