module Common.Solver where

import Data.Set (Set, size, filter, toList)
import Data.Map (Map, elems)
import Data.Either (isRight, rights)

optionSolver :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b)
optionSolver i
    | and . fmap isRight . elems $ p = Just . fmap (\(Right a) -> a) $ p
    | otherwise = Nothing
    where
        p = go . fmap Left $ i
        go j = if j == n then j else go n where
            n = fmap iter $ j
            iter (Left x)
                | size x == 1 = Right . head . toList $ x
                | otherwise = Left . Data.Set.filter ((flip notElem) . rights . elems $ j) $ x
            iter q = q
