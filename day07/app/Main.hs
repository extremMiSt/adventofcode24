module Main where
import Data.List.Split (splitOn)

records :: [Char] -> (Integer, [Integer])
records l = (goal, list)
    where 
        (x:xs) = splitOn ": "  l
        goal = read x
        list = map read $ (words.head) xs

isValid :: (Ord t, Num t) => (t, [t]) -> Bool
isValid (g, l) = isValid' g 0 l
    where
        isValid' goal cur [] | cur == goal = True
                             | otherwise = False
        isValid' goal cur (n:ns) | goal <= cur = False
                                 | otherwise = isValid' goal (cur+n) ns || isValid' goal (cur*n) ns

isValider :: (Read t, Show t, Ord t, Num t) => (t, [t]) -> Bool
isValider (g, l) = isValid' g 0 l
    where
        isValid' goal cur [] | cur == goal = True
                             | otherwise = False
        isValid' goal cur (n:ns) | goal <= cur = False
                                 | otherwise = isValid' goal (cur+n) ns || 
                                                isValid' goal (cur*n) ns || 
                                                isValid' goal (read (show cur ++ show n)) ns

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let calibrations = map records $ lines f
    print $ (sum.map fst) $ filter isValid calibrations
    print $ (sum.map fst) $ filter isValider calibrations
