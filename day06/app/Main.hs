module Main where
import Data.List (nub)
import Debug.Trace (trace)

s :: Int
s = 130

obstacles :: [[Char]] -> Int -> Int -> [(Int, Int)]
obstacles (('#':row):line) x y = (x,y): obstacles (row:line) (x+1) y 
obstacles ((_:row):line) x y = obstacles (row:line) (x+1) y
obstacles ([]:line) _ y = obstacles line 0 (y+1)  
obstacles [] _ _ = []

start :: [[Char]] -> Int -> Int -> (Int, Int, Char)
start ((a:row):line) x y | a == '^' || a == 'v' || a == '>' || a== '<' = (x,y, a)
                    | otherwise = start (row:line) (x+1) y
start ([]:line) _ y = start line 0 (y+1)  
start [] _ _ = (-1,-1, '#')

step :: [(Int, Int)] -> (Int, Int, Char) -> (Int, Int, Char)
step obst (x,y,dir) | dir == '^' && (x,y-1) `elem` obst = (x,y, '>')
                    | dir == '^' = (x,y-1, '^')
                    | dir == '>' && (x+1,y) `elem` obst = (x,y, 'v')
                    | dir == '>' = (x+1,y, '>')
                    | dir == 'v' && (x,y+1) `elem` obst = (x,y, '<')
                    | dir == 'v' = (x,y+1, 'v')
                    | dir == '<' && (x-1,y) `elem` obst = (x,y, '^')
                    | dir == '<' = (x-1,y, '<')
                    | otherwise = error "eh"
                    
allSteps :: [(Int, Int)] -> (Int, Int, Char) -> [(Int, Int)]
allSteps obst (x,y,dir) | x2 < 0 || y2 < 0 || x2 >= s || y2 >= s = []
                        | otherwise = (x2,y2) : allSteps obst (x2,y2,dir2)
                            where
                                (x2,y2,dir2) = step obst (x,y,dir)

task1 :: [(Int, Int)] -> (Int, Int, Char) -> Int
task1 obst (x,y,dir) = length $ nub $ (x,y): allSteps obst (x,y,dir)

showPath :: (Foldable t1, Foldable t2) => t1 (Int, Int) -> t2 (Int, Int) -> Int -> Int -> [Char]
showPath field path x y | y > s = []
                        | x > s = '\n': showPath field path (-1) (y+1)
                        | (x,y) `elem` field = '#': showPath field path (x+1) y
                        | (x,y) `elem` path = '+': showPath field path (x+1) y
                        | otherwise = '.': showPath field path (x+1) y

allSteps' ::(Int, Int, Char) -> [(Int, Int, Char)] -> [(Int, Int)] -> Bool
allSteps' (x,y,dir) seen obst | x < 0 || y < 0 || x >= s || y >= s = False
                              | (x2,y2, dir2) `elem` seen = True
                              | otherwise = allSteps'  (x2,y2,dir2) ((x2,y2,dir2):seen) obst
                                where
                                    (x2,y2,dir2) = step obst (x,y,dir)

task2 :: [(Int, Int)] -> (Int, Int, Char) -> Int
task2 obst (x,y,dir) = length $ filter ((\a -> trace (show a) a) . allSteps' (x,y,dir) []) (map (:obst) normal)
    where 
        normal = nub $ (x,y): allSteps obst (x,y,dir)

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let field = lines f
    let obst = obstacles field 0 0
    let pos = start field 0 0
    print $ task1 obst pos
    print $ task2 obst pos
