{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

records :: Read b => String -> [[b]]
records l = map (map read.words) $ lines l

diff :: Num c => [c] -> [c]
diff (x:xs) = zipWith (-) (x:xs) xs
diff [] = [];

count :: Num b => [Bool] -> b
count xs = fromIntegral $ length (filter id xs)

isSafe :: (Foldable t, Ord a, Num a) => t a -> Bool
isSafe r = (all (< 0) r || all (> 0) r) && all (\x -> abs x <= 3) r

vars :: [a] -> [[a]]
vars l = impl l []
    where
        impl (x:xs) p = (p++xs) : impl xs (p++[x])
        impl [] p = [p]

task1 :: (Foldable t, Ord a, Num b, Num a) => [t a] -> b
task1 l = count $ map isSafe l

isVarSafe :: (Ord a, Num a) => [a] -> Bool
isVarSafe v = any (isSafe . diff) (vars v)

task2 :: (Ord a, Num b, Num a) => [[a]] -> b
task2 l = count $ map isVarSafe l

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let record = records f;
    print $ task1 $ map diff record
    print $ task2 record
