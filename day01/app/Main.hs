module Main where
import qualified Data.List as List

split :: [a] -> ([a], [a])
split s = (take 5 s, drop 8 s)

count :: (Num b, Eq a) => [a] -> a -> b
count xs find = fromIntegral $ length (filter (== find) xs)

first :: (Ord a, Read a) => [String] -> [a]
first l = List.sort (map (read.fst.split) l)

second :: (Ord a, Read a) => [String] -> [a]
second l = List.sort (map (read.snd.split) l)

task1 :: (Num a, Ord a, Read a) => [String] -> a
task1 l = sum $ map abs $ zipWith (-) (first l) (second l)

task2 :: (Num a, Ord a, Read a) => [String] -> a
task2 l = sum $ map (\x -> x * count (second l) x) (first l)

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    print $ task1 $ lines f
    print $ task2 $ lines f

