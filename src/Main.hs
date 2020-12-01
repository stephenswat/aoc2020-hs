import System.Environment

import Common

import qualified Day01

main :: IO ()
main = do {
    args <- getArgs;
    file <- readFile (head args);
    let (solA, solB) = Day01.solution in do {
        putStrLn . ("Problem 1: " ++) . solA $ file;
        putStrLn . ("Problem 2: " ++) . solB $ file;
    }
}
