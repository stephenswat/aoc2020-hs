module Problems.Day02 (solution) where

import Text.Regex.PCRE

import Common.Solution (Day)

type PwdReq = (Int, Int, Char, String)

parsePwdReq :: String -> PwdReq
parsePwdReq s = (read l, read u, head c, r)
    where (_:l:u:c:r:[]) = head (s =~ "(\\d+)-(\\d+) (\\w): (\\w+)" :: [[String]])

isValidA :: PwdReq -> Bool
isValidA (l, u, c, s) = l <= count && count <= u
    where count = length . filter (c ==) $ s

isValidB :: PwdReq -> Bool
isValidB (l, u, c, s) = (valid l) /= (valid u)
    where valid n = s !! (n - 1) == c

solution :: Day
solution = (toSolution isValidA, toSolution isValidB)
    where toSolution f = show . length . filter f . map parsePwdReq . lines
