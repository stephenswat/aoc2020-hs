module Problems.Day06 (solution) where

import Data.Set (fromList, unions, size, intersection)
import Data.List.Split (splitWhen)

import Common.Solution (Day)

solution :: Day
solution =
    ( show . sum . map (size . unions . map fromList) . splitWhen (== "") . lines
    , show . sum . map (size . foldl1 intersection . map fromList) . splitWhen (== "") . lines
    )
