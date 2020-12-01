import System.Environment

main :: IO ()
main = do {
    args <- getArgs;
    file <- readFile (head args);
    let nums = map (read :: String -> Integer) . lines $ file
        ans1 = [a * b | a <- nums, b <- nums, a + b == 2020]
        ans2 = [a * b * c | a <- nums, b <- nums, c <- nums, a + b + c == 2020]
    in do {
        putStrLn . ("Problem 1: " ++) . show . head $ ans1;
        putStrLn . ("Problem 2: " ++) . show . head $ ans2;
    }
}
