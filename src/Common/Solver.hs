module Common.Solver where

import Data.Set (Set, size, empty, filter, toList, unions)
import Data.Map (Map, fromList, elems, toList, map)
import Data.Maybe (isJust)

optionSolver :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b)
optionSolver = go . Data.Map.map (\x -> (Nothing, x))
    where
        go :: (Ord a, Ord b) => Map a (Maybe b, Set b) -> Maybe (Map a b)
        go i
            | done      = Just (Data.Map.fromList [(j, m) | (j, (Just m, _)) <- Data.Map.toList i])
            | i == n    = Nothing
            | otherwise = go n
            where
                reduce f (Nothing, xs)
                    | size xs == 1 = (Just (head . Data.Set.toList $ xs), empty)
                    | otherwise = (Nothing, Data.Set.filter ((flip notElem) f) xs)
                reduce _ j                 = j
                done = and [isJust . fst $ j | j <- elems i]
                finished = unions . Prelude.filter ((== 1) . size) . Prelude.map snd . elems $ i
                n = Data.Map.map (reduce finished) $ i