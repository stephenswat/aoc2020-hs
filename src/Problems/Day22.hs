module Problems.Day22 (solution) where

import Text.Parsec.Prim (parse)
import Text.Parsec.Char (char, string, digit)
import Text.Parsec.Combinator (sepEndBy1, many1)
import Text.Parsec.String (Parser)
import Data.Either (fromRight)
import Data.Set (Set, empty, member, insert)

import Common.Solution (Day)

type Deck = [Int]

data Combat = Combat
    { p1 :: Deck
    , p2 :: Deck
    , hist :: Set (Deck, Deck)
    } deriving (Eq)

data Game a = Player1 a | Player2 a | Ongoing a

player1Win :: Game a -> Bool
player1Win (Player1 _) = True
player1Win _ = False

parseInput :: Parser (Game Combat)
parseInput = do
    _  <- string "Player 1:\n";
    d1 <- sepEndBy1 number (char '\n')
    _  <- string "\nPlayer 2:\n";
    d2 <- sepEndBy1 number (char '\n')
    return $ Ongoing (Combat{p1=d1, p2=d2, hist=empty})
    where
        number = read <$> many1 digit

stepGameA :: Combat -> Game Combat
stepGameA s@Combat{p1=(t1:d1), p2=(t2:d2)}
    | t1 >= t2  = Ongoing s{p1=d1 ++ [t1, t2], p2=d2}
    | otherwise = Ongoing s{p1=d1, p2=d2 ++ [t2, t1]}
stepGameA s@Combat{p1=[]} = Player2 s
stepGameA s@Combat{p2=[]} = Player1 s

stepGameB :: Combat -> Game Combat
stepGameB s@Combat{p1=(t1:d1), p2=(t2:d2), hist=h}
    | member (n1, n2) h = Player1 s
    | otherwise = Ongoing s{p1=n1, p2=n2}
    where
        (n1, n2) = if p1win then (d1 ++ [t1, t2], d2) else (d1, d2 ++ [t2, t1])
        p1win = if (t1 <= length d1) && (t2 <= length d2) then
            player1Win . playGame stepGameB $ (Ongoing Combat{p1=take t1 d1, p2=take t2 d2, hist=empty})
        else
            t1 >= t2
stepGameB s@Combat{p1=[]} = Player2 s
stepGameB s@Combat{p2=[]} = Player1 s

playGame :: (Combat -> Game Combat) -> Game Combat -> Game Combat
playGame f (Ongoing g) = case f g of
    (Ongoing h) -> playGame f (Ongoing h{hist=insert ((p1 h), (p2 h)) (hist h)})
    i -> i
playGame _ _ = error "Cannot step completed game!"

winner :: Game Combat -> Maybe Deck
winner (Player1 Combat{p1=d}) = Just d
winner (Player2 Combat{p2=d}) = Just d
winner (Ongoing _) = Nothing

score :: Deck -> Int
score = sum . map (uncurry (*)) . zip [1..] . reverse

solution :: Day
solution =
    ( show . fmap score . winner . playGame stepGameA . getState
    , show . fmap score . winner . playGame stepGameB . getState
    ) where getState = fromRight (error "Bad read!") . parse parseInput ""
