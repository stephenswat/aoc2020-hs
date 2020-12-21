{-# LANGUAGE TupleSections #-}

module Problems.Day19 (solution) where

import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Control.Monad.Zip (mzip)
import Control.Monad (forM)
import Data.Tuple.Extra (both)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, lookup, insert)

import Common.Solution (Day)

data Expression
    = Seq Expression Expression
    | Alt Expression Expression
    | Con Char
    | Rec Expression Expression
    | Emp

readExpr :: Map Integer String -> String -> Maybe Expression
readExpr n j = pAlt n j <|> pRec n j <|> pSeq n j <|> pRef n j <|> pCon n j
    where
        pRef m i = readMaybe i >>= (flip Data.Map.lookup) m >>= readExpr m
        pRec m i = (z r) <&> drop 1 <&> (init l,) <&> both q <&> uncurry Rec
            where
                (l, r) = break (== '_') $ i
                q x = fromMaybe Emp (readExpr m x)
                z ('_':o) = Just o
                z _ = Nothing
        pAlt m i = case splitOn " | " i of
            l@(_:_:_) -> forM l (readExpr m) <&> foldl1 Alt
            _ -> Nothing
        pSeq m i = case splitOn " " i of
            l@(_:_:_) -> forM l (readExpr m) <&> foldl1 Seq
            _ -> Nothing
        pCon _ ['"', c, '"'] = Just (Con c)
        pCon _ _ = Nothing

match :: Expression -> String -> Bool
match e = elem "" . go e
    where
        go (Seq l r) s = (go l s) >>= (go r)
        go (Alt l r) s = (go l s) <|> (go r s)
        go (Con c) s
            | (not . null $ s) && c == (head s) = [tail s]
            | otherwise = []
        go c@(Rec l r) s = ((go l s) >>= (go r)) <|> ((go l s) >>= (go c) >>= (go r))
        go Emp s = [s]

rawInput :: String -> (Map Integer String, [String])
rawInput i = (fromList . map splitRule . lines $ r, lines s)
    where
        [r, s] = splitOn "\n\n" i
        splitRule l = let (n:m:_) = splitOn ": " l in (read n, m)

solution :: Day
solution =
    ( q id
    , q (first (insert 11 "42 _ 31" . insert 8 "42 _"))
    ) where
        p (r, l) = ((Data.Map.lookup 0 r) >>= readExpr r <&> (,l))
        m (e, l) = map (match e) $ l
        q f = show . length . filter id . m . fromJust . p . f . rawInput
