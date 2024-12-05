module Main where
import Data.List.Split (splitOn)
import Data.List (sortBy)

sep :: String -> String -> (String, String)
sep ('\n':'\n':xs) s = (s++"\n",xs)
sep (x:xs) s = sep xs (s++[x])
sep [] s = (s, "")

match :: Eq a => [a] -> Bool -> Bool -> [a] -> Bool
match (a:line) seen0 seen1 rule | a == rule!!1 && seen0 = True
                                | a == rule!!1 = match line False True rule
                                | a == rule!!0 && seen1 = False
                                | a == rule!!0 = match line True False rule
                                | otherwise = match line seen0 seen1 rule
match [] _ _ _ = True

matchAll :: (Foldable t, Eq a) => t [a] -> [a] -> Bool
matchAll rules line = all (match line False False) rules

middle:: [Integer] -> Integer
middle l = l!!(length l `div` 2)

part1 :: [[Integer]] -> [[Integer]] -> Integer
part1 line rules = sum $ map middle $ filter (matchAll rules) line

order :: (Foldable t, Eq a) => t [a] -> a -> a -> Ordering
order rules i j | [i,j] `elem` rules = GT
                | [j,i] `elem` rules = LT
                | otherwise = EQ

part2 :: [[Integer]] -> [[Integer]] -> Integer
part2 line rules = sum $ map (middle . sortBy (order rules)) $ filter (not . matchAll rules) line

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let parts = sep f []
    let rules = map (map read . splitOn "|") $ lines $ fst parts
    let updates = map (map read . splitOn ",") $ lines $ snd parts
    print $ part1 updates rules
    print $ part2 updates rules