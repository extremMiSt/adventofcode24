module Main where

records :: String -> [[Integer]]
records l = map (map read.words) $ lines l

diff :: [Integer] -> [Integer]
diff (x:xs) = zipWith (-) (x:xs) xs
diff [] = [];

count :: [Bool] -> Integer
count xs = fromIntegral $ length (filter id xs)

isSafe :: [Integer] -> Bool
isSafe r = (all (< 0) r || all (> 0) r) && all (\x -> abs x <= 3) r

vars :: [a] -> [[a]]
vars l = impl l []
    where
        impl (x:xs) p = (p++xs) : impl xs (p++[x])
        impl [] p = [p]

task1 :: [[Integer]] -> Integer
task1 l = count $ map isSafe l

isVarSafe :: [Integer] -> Bool
isVarSafe v = any (isSafe . diff) (vars v)

task2 :: [[Integer]] -> Integer
task2 l = count $ map isVarSafe l


main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let record = records f;
    print $ task1 $ map diff record
    print $ task2 record
