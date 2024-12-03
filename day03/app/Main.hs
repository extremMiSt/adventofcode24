module Main where
import Debug.Trace (trace)

pairs :: String -> [[String]]
pairs f = map words $ lines f

task1 :: [[String]] -> Integer
task1 ([a,b]:xs) = read a*read b + task1 xs
task1 [] = 0;
task1 x = trace ("fail"++show x) 0

task2 :: Bool -> [[String]] -> Integer
task2 _ (["do()"]:xs) = task2 True xs
task2 _ (["don't()"]:xs) = task2 False xs
task2 True ([a,b]:xs) = read a*read b + task2 True xs
task2 False ([_,_]:xs) = task2 False xs
task2 _ [] = 0
task2 x y= trace ("fail"++show x++show y) 0

main :: IO ()
main = do
    f <- readFile "./input02.txt"
    print $ task1 $ pairs f;

    g<- readFile "./input03.txt" --mul\((\d*),(\d*)\)|(do\(\))|(don't\(\))
    print $ task2 True $ pairs g;
