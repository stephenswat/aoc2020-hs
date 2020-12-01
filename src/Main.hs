import System.Environment

import Common

import qualified Problems.Day01

solutions :: [Day]
solutions = [Problems.Day01.solution]

main :: IO ()
main = do {
    args <- getArgs;
    let
        dayn = read (args !! 0) :: Integer
        sols = solutions !! dayn
        fnam = args !! 1
    in do {
        file <- readFile (head args);
        let (solA, solB) = sols in do {
            putStrLn . ("Problem 1: " ++) . solA $ file;
            putStrLn . ("Problem 2: " ++) . solB $ file;
        }
    }
}
