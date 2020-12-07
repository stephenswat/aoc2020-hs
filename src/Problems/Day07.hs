module Problems.Day07 (solution) where

import Data.Map (Map, fromList, lookup, keys)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isNothing, fromJust)

import Common.Solution (Day)

type Colour = String
type BagContents = Map Colour [(Integer, Colour)]

parseBag :: String -> (Colour, [(Integer, Colour)])
parseBag s = (o, catMaybes . map (parseInnerBag . splitOn " ") . splitOn ", " $ i)
    where
        [o, i] = splitOn " bags contain " s
        parseInnerBag :: [String] -> Maybe (Integer, Colour)
        parseInnerBag (n:a:c:_)
            | n == "no" = Nothing
            | otherwise = Just (read n, a ++ " " ++ c)

canContain :: Colour -> BagContents -> Colour -> Bool
canContain n h c
    | isNothing q = False
    | otherwise   = or [c' == n || canContain n h c' | (_, c') <- fromJust q]
    where q = Data.Map.lookup c h

countInnerBags :: Colour -> BagContents -> Integer
countInnerBags c m
    | isNothing q = 0
    | otherwise   = sum [n * (1 + countInnerBags c' m) | (n, c') <- fromJust q]
    where q = Data.Map.lookup c m

solution :: Day
solution =
    ( show . length . (\m -> filter (canContain "shiny gold" m) . keys $ m) . parse
    , show . countInnerBags "shiny gold" . parse
    ) where parse = fromList . map parseBag . lines
