import System.Environment

import Common.Solution (Day)

import qualified Problems.Day01
import qualified Problems.Day02
import qualified Problems.Day03
import qualified Problems.Day04
import qualified Problems.Day05
import qualified Problems.Day06
import qualified Problems.Day07

solutions :: [Day]
solutions =
    [ Problems.Day01.solution
    , Problems.Day02.solution
    , Problems.Day03.solution
    , Problems.Day04.solution
    , Problems.Day05.solution
    , Problems.Day06.solution
    , Problems.Day07.solution
    ]

main :: IO ()
main = do {
    args <- getArgs;
    let
        dayn = read (args !! 0) :: Int
        sols = solutions !! (dayn - 1)
        fnam = args !! 1
    in do {
        putStrLn ("Solutions for day " ++ show dayn);
        file <- readFile fnam;
        let (solA, solB) = sols in do {
            putStrLn . ("Problem A: " ++) . solA $ file;
            putStrLn . ("Problem B: " ++) . solB $ file;
        }
    }
}
