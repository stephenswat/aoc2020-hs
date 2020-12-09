module Problems.Day04 (solution) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Text.Regex.PCRE

import Common.Solution (Day)

data Passport = Passport {
    byr :: Maybe String,
    iyr :: Maybe String,
    eyr :: Maybe String,
    hgt :: Maybe String,
    hcl :: Maybe String,
    ecl :: Maybe String,
    pid :: Maybe String,
    cid :: Maybe String
} deriving (Show)

emptyPassport :: Passport
emptyPassport = Passport {
    byr=Nothing,
    iyr=Nothing,
    eyr=Nothing,
    hgt=Nothing,
    hcl=Nothing,
    ecl=Nothing,
    pid=Nothing,
    cid=Nothing
}

readFields :: [(String, String)] -> Passport
readFields [] = emptyPassport
readFields (("byr", v):xs) = (readFields xs) { byr=Just v }
readFields (("iyr", v):xs) = (readFields xs) { iyr=Just v }
readFields (("eyr", v):xs) = (readFields xs) { eyr=Just v }
readFields (("hgt", v):xs) = (readFields xs) { hgt=Just v }
readFields (("hcl", v):xs) = (readFields xs) { hcl=Just v }
readFields (("ecl", v):xs) = (readFields xs) { ecl=Just v }
readFields (("pid", v):xs) = (readFields xs) { pid=Just v }
readFields (("cid", v):xs) = (readFields xs) { cid=Just v }
readFields (_:xs) = readFields xs

readPassports :: String -> [Passport]
readPassports
    = map (readFields . catMaybes . map split' . splitOn " " . intercalate " ")
    . splitWhen (== "")
    . lines
    where
        split' (a:b:c:':':xs) = Just ([a, b, c], xs)
        split' _ = Nothing

isValidA :: Passport -> Bool
isValidA p = and [isJust . x $ p | x <- fields]
    where fields = [byr, iyr, eyr, hgt, hcl, ecl, pid]

validatebyr :: String -> Bool
validatebyr s = let y = (read s) :: Integer in y >= 1920 && y <= 2002

validateiyr :: String -> Bool
validateiyr s = let y = (read s) :: Integer in y >= 2010 && y <= 2020

validateeyr :: String -> Bool
validateeyr s = let y = (read s) :: Integer in y >= 2020 && y <= 2030

validatehgt :: String -> Bool
validatehgt s
    | u == "cm" = n >= 150 && n <= 193
    | u == "in" = n >= 59 && n <= 76
    | otherwise = False
    where
        (m, u) = span isDigit s
        n = read m :: Integer

validatehcl :: String -> Bool
validatehcl s = (s =~ "^#[a-f0-9]{6}$" :: Bool)

validateecl :: String -> Bool
validateecl "amb" = True
validateecl "blu" = True
validateecl "brn" = True
validateecl "gry" = True
validateecl "grn" = True
validateecl "hzl" = True
validateecl "oth" = True
validateecl _ = False

validatepid :: String -> Bool
validatepid s = (s =~ "^[0-9]{9}$" :: Bool)

isValidB :: Passport -> Bool
isValidB Passport {
    byr=Just byr',
    iyr=Just iyr',
    eyr=Just eyr',
    hgt=Just hgt',
    hcl=Just hcl',
    ecl=Just ecl',
    pid=Just pid',
    cid=_
} = validatebyr byr' && validateiyr iyr' && validateeyr eyr' && validatehgt hgt' &&
    validatehcl hcl' && validateecl ecl' && validatepid pid'
isValidB _ = False

solution :: Day
solution =
    ( show . length . filter isValidA . readPassports
    , show . length . filter isValidB . readPassports
    )
