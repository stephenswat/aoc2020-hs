module Common.Utils where

import Data.Map (Map, insertWith, empty)

count :: (Ord a, Integral b) => [a] -> Map a b
count = foldr (\a n -> insertWith (+) a 1 n) empty