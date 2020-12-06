module Problems.Day06 (solution) where

import Data.Set (fromList, unions, size, intersection)
import Data.List.Split (splitWhen)

import Common.Solution (Day)

solution :: Day
solution =
    ( q (size . unions . map fromList)
    , q (size . foldl1 intersection . map fromList)
    ) where q f = show . sum . map f . splitWhen null . lines
