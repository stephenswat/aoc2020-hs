module Problems.Day18 (solution) where

import Data.Char (isDigit)
import Data.Maybe (catMaybes)

import Common.Solution (Day)

data Token = Number Integer | Add | Mul | OpenPar | ClosePar

lexChar :: Char -> Maybe Token
lexChar '+' = Just (Add)
lexChar '*' = Just (Mul)
lexChar '(' = Just (OpenPar)
lexChar ')' = Just (ClosePar)
lexChar n
    | isDigit n = Just (Number (read [n]))
    | otherwise = Nothing

flattenAdd :: [Token] -> [Token]
flattenAdd (Number l:Add:Number r:e) = flattenAdd ((Number (l + r)):e)
flattenAdd (l:e) = l:(flattenAdd e)
flattenAdd [] = []

flattenLtR :: [Token] -> Integer
flattenLtR (Number l:Add:e) = l + flattenLtR e
flattenLtR (Number l:Mul:e) = l * flattenLtR e
flattenLtR (Number l:[])    = l
flattenLtR _                = error "Invalid flatten"

calc :: ([Token] -> Integer) -> [Token] -> Integer
calc f t = case reduce [] t of
    ([Number r], []) -> r
    _                 -> error "Error in reduction"
    where
        reduce s []           = ([Number (f s)], [])
        reduce s (OpenPar:r)  = let (q, u) = reduce [] r in reduce (q ++ s) u
        reduce s (ClosePar:r) = let (q, _) = reduce s [] in (q, r)
        reduce s (c:r)        = reduce (c:s) r

solution :: Day
solution =
    ( q flattenLtR
    , q (flattenLtR . flattenAdd)
    ) where q f = show . sum . map (calc f) . map (catMaybes . map lexChar) . lines
