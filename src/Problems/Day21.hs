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

import Common.Solver (optionSolver)
import Common.Solution (Day)

parseLine :: Parser (Set String, Set String)
parseLine = do
    foods <- endBy1 (many1 letter) (char ' ');
    _ <- string "(contains ";
    allergens <- sepBy1 (many1 letter) (string ", ");
    return (Data.Set.fromList foods, Data.Set.fromList allergens)

initMap :: [(Set String, Set String)] -> Map String (Set String)
initMap i = Data.Map.fromList [(x, allFood) | x <- toList allAllergens]
    where
        allFood = unions . map fst $ i
        allAllergens = unions . map snd $ i

f :: Map String (Set String) -> (Set String, Set String) -> Map String (Set String)
f c (q, a) = foldl (\c' a' -> adjust (intersection q) a' c') c a

solveA :: [(Set String, Set String)] -> Int
solveA i = length . filter ((flip member) noncandidates) $ allFoodList
    where
        candidates = unions . elems . foldl f (initMap i) $ i
        noncandidates = difference (unions . map fst $ i) candidates
        allFoodList = concat . map toList . map fst $ i

solveB :: [(Set String, Set String)] -> String
solveB i = intercalate "," . elems . fromJust . optionSolver $ (foldl f (initMap i) i)

solution :: Day
solution =
    ( show . solveA . rights . map (parse parseLine "") . lines
    , show . solveB . rights . map (parse parseLine "") . lines
    )
