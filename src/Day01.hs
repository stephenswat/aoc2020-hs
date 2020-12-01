import System.Environment

type Solution = String -> String

parseNumbers :: String -> [Integer]
parseNumbers = map read . lines

problemA :: [Integer] -> Integer
problemA i = head [a * b | a <- i, b <- i, a + b == 2020]

problemB :: [Integer] -> Integer
problemB i = head [a * b * c | a <- i, b <- i, c <- i, a + b + c == 2020]

day01 :: (Solution, Solution)
day01 = (show . problemA . parseNumbers, show . problemB . parseNumbers)

main :: IO ()
main = do {
    args <- getArgs;
    file <- readFile (head args);
    let (solA, solB) = day01 in do {
        putStrLn . ("Problem 1: " ++) . solA $ file;
        putStrLn . ("Problem 2: " ++) . solB $ file;
    }
}
