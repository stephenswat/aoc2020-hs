module Problems.Day21 (solution) where

import Text.Parsec.Prim (parse)
import Text.Parsec.Char (letter, char, string)
import Text.Parsec.Combinator (endBy1, sepBy1, many1)
import Text.Parsec.String (Parser)
import Data.Either (rights)
import Data.Map (Map, fromList, adjust, elems)
import Data.Set (Set, fromList, unions, toList, intersection, difference, member)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import Control.Arrow ((&&&))

import Common.Solver (optionSolver)
import Common.Solution (Day)

parseLine :: Parser (Set String, Set String)
parseLine = do
    foods <- endBy1 (many1 letter) (char ' ');
    _ <- string "(contains ";
    allergens <- sepBy1 (many1 letter) (string ", ");
    return (Data.Set.fromList foods, Data.Set.fromList allergens)

initMap :: [(Set String, Set String)] -> Map String (Set String)
initMap i = Data.Map.fromList [(x, allF) | x <- toList allA]
    where (allF, allA) = both unions . unzip $ i

presolve :: [(Set String, Set String)] -> Map String (Set String)
presolve i = foldr f (initMap i) i
    where
        f (b, a) c = foldr (adjust (intersection b)) c a

solveA :: [(Set String, Set String)] -> Int
solveA i = length . filter ((flip member) nc) . concat . map (toList . fst) $ i
    where
        nc = uncurry difference . both unions . (map fst &&& elems . presolve) $ i

solveB :: [(Set String, Set String)] -> String
solveB = intercalate "," . elems . fromJust . optionSolver . presolve

solution :: Day
solution =
    ( show . solveA . rights . map (parse parseLine "") . lines
    , show . solveB . rights . map (parse parseLine "") . lines
    )
