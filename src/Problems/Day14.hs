{-# LANGUAGE LambdaCase #-}

module Problems.Day14 (solution) where

import Data.Map (Map, empty, insert, elems)
import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import Data.Bits

import Common.Solution (Day)

data Operation
    = Store Integer Integer
    | Mask [MaskBit]
    deriving (Show)

data MaskBit
    = Set
    | Unset
    | Float
    deriving (Show)

data Memory = Memory {
    mem :: Map Integer Integer,
    msk :: [MaskBit]
}

parseOperation :: String -> Operation
parseOperation i
    | s == "mask" = parseMsk xs
    | s == "mem"  = parseMem xs
    | otherwise = error "Not a mask or a mem!"
    where
        (s:xs) = concat . map (splitOn "[") . splitOn " = " $ i
        parseMem (a:v:[]) = Store (read . init $ a) (read v)
        parseMem _ = error "Invalid store command!"
        parseMsk (j:[]) = Mask . reverse . map parseBit $ j
        parseMsk _ = error "Invalid mask command!"

parseBit :: Char -> MaskBit
parseBit '0' = Unset
parseBit '1' = Set
parseBit 'X' = Float
parseBit _   = error "Invalid mask bit!"

foldMask :: [MaskBit] -> Integer -> Integer
foldMask i
    = foldl1 (.)
    . map (\(j, f) -> (flip f) j)
    . map (second (\case {Unset -> clearBit; Set -> setBit; Float -> const}))
    . zip [0..]
    $ i

memMask :: [MaskBit] -> Integer -> [Integer]
memMask m i = go m i 0 where
    go []        j _ = [j]
    go (Set  :x) j d = go x (setBit j d) (d + 1)
    go (Unset:x) j d = go x (const j d) (d + 1)
    go (Float:x) j d = (go x (setBit j d) (d + 1)) ++ (go x (clearBit j d) (d + 1))

initMemory :: Memory
initMemory = Memory{mem=empty, msk=[]}

applyOpA :: Memory -> Operation -> Memory
applyOpA m (Store p v) = m{mem=insert p ((foldMask $ msk m) $ v) (mem m)}
applyOpA m (Mask n) = m{msk=n}

applyOpB :: Memory -> Operation -> Memory
applyOpB m (Store p v) = m{mem=foldl (\m' p' -> insert p' v m') (mem m) . memMask (msk m) $ p}
applyOpB m (Mask n) = m{msk=n}

solution :: Day
solution =
    ( q applyOpA
    , q applyOpB
    ) where q f = show . sum . elems . mem . foldl f initMemory . map parseOperation . lines