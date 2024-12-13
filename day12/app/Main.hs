{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import Debug.Trace (trace)
import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (><))
import qualified Data.Set as S
import Data.List (delete)

data Dir = NORTH | SOUTH | EAST | WEST
    deriving (Eq,Ord,Show)

class Ord a => Graph b a where
    neighbours :: b -> a -> [a]

reacheableBFS :: Graph b a => b -> S.Set a -> Seq a -> S.Set a
reacheableBFS _ visited Q.Empty = visited
reacheableBFS graph visited (e:<|q) | e `elem` visited = reacheableBFS graph visited q
                                    | otherwise = reacheableBFS graph (S.insert e visited) (q >< Q.fromList (neighbours graph e))

trace' :: (Show b, Show a) => b -> a -> a
trace' b a = trace (show b ++ show a) a

trace2 :: Show a =>  a -> a
trace2 a = trace (show a) a

instance Graph  [[Char]] (Int,Int) where
    neighbours :: [[Char]] -> (Int,Int) -> [(Int,Int)]
    neighbours area (xp,yp) = north ++ south ++ east ++ west
        where
            north = [(xp-1,yp) | (xp-1) >= 0 && area!!yp!!(xp-1) == area!!yp!!xp]
            south = [(xp+1,yp) | (xp+1) < length (head area) && area!!yp!!(xp+1) == area!!yp!!xp]
            east = [(xp,yp-1) | (yp-1) >= 0 && area!!(yp-1)!!xp == area!!yp!!xp]
            west = [(xp,yp+1) | (yp+1) < length (head area) && area!!(yp+1)!!xp == area!!yp!!xp]

perimeter :: Eq a => Int -> [[a]] -> (Int,Int) -> Int
perimeter len inp (x,y) = length (north++south++east++west)
    where
        north = [(x-1,y) | x-1 < 0 || x-1 >= 0 && inp!!y!!x /= inp!!y!!(x-1)]
        south = [(x+1,y) | x+1 >= len || inp!!y!!x /= inp!!y!!(x+1)]
        east = [(x,y-1) | y-1 < 0 || y-1 >= 0 && inp!!y!!x /= inp!!(y-1)!!x]
        west = [(x,y+1) | y+1 >= len || y+1 < len && inp!!y!!x /= inp!!(y+1)!!x]

partition :: Int -> [[Char]] -> [[Char]] -> Int -> Int -> S.Set (Int,Int) -> [(Char, S.Set (Int, Int))] -> [(Char, S.Set (Int, Int))]
partition len inp ((c:xs):ys) x y seen part | (x,y) `S.member` seen = partition len inp (xs:ys) (x+1) y seen part
                                            | otherwise = partition len inp (xs:ys) (x+1) y (seen `S.union` news) ((c,news):part)
    where
        news = reacheableBFS inp S.empty (Q.singleton (x,y))
partition len inp ([]:ys) _ y seen part = partition len inp ys 0 (y+1) seen part
partition _ _ [] _ _ _ part = part

score :: Int -> [[Char]] -> [(Char, S.Set (Int, Int))] -> Integer
score len area elements = sum $ map (\(_,x) -> fromIntegral (S.size x) * fromIntegral (sum $ map (perimeter len area) (S.toList x))) elements

part1 :: Int -> [[Char]] -> Integer
part1 len input = score len input (partition len input input 0 0 S.empty [])

sides :: Int -> [[Char]] -> (Int, Int) -> [(Dir, Int,Int)]
sides len inp (x,y) = north++south++east++west
    where
        west = [(WEST,x,y) | x-1 < 0 || x-1 >= 0 && inp!!y!!x /= inp!!y!!(x-1)]
        east = [(EAST,x,y) | x+1 >= len || inp!!y!!x /= inp!!y!!(x+1)]
        north = [(NORTH,x,y) | y-1 < 0 || y-1 >= 0 && inp!!y!!x /= inp!!(y-1)!!x]
        south = [(SOUTH,x,y) | y+1 >= len || y+1 < len && inp!!y!!x /= inp!!(y+1)!!x]

allSides :: Int -> [String] -> S.Set (Int,Int) -> [(Dir, Int, Int)]
allSides len inp elems = concatMap (sides len inp) (S.toList elems)

compressSides :: (Dir, Int, Int) -> [(Dir, Int, Int)] -> Int
compressSides _ [] = 0

compressSides (NORTH,x,y) todo
                             | (NORTH,x-1,y) `elem` todo = compressSides (NORTH,x-1,y) ((NORTH,x-1,y) `delete` todo)
                             | (WEST,x,y) `elem` todo = 1+ compressSides (WEST,x,y) ((WEST,x,y) `delete` todo)
                             | (EAST,x-1,y-1) `elem` todo = 1+ compressSides (EAST,x-1,y-1) ((EAST,x-1,y-1) `delete` todo)
                             | otherwise = 1 + compressSides start sides'
    where
        start = head $ filter (\(s,xp,yp) -> s==NORTH && (s,xp+1,yp) `notElem` todo) todo
        sides' = start `delete` todo

compressSides (SOUTH,x,y) todo
                             | (SOUTH,x+1,y) `elem` todo = compressSides (SOUTH,x+1,y) ((SOUTH,x+1,y) `delete` todo)
                             | (EAST,x,y) `elem` todo = 1+ compressSides (EAST,x,y) ((EAST,x,y) `delete` todo)
                             | (WEST,x+1,y+1) `elem` todo = 1+ compressSides (WEST,x+1,y+1) ((WEST,x+1,y+1) `delete` todo)
                             | otherwise = 1 + compressSides start sides'
    where
        start = head $ filter (\(s,xp,yp) -> s==NORTH && (s,xp+1,yp) `notElem` todo) todo
        sides' = start `delete` todo

compressSides (EAST,x,y) todo
                             | (EAST,x,y-1) `elem` todo = compressSides (EAST,x,y-1) ((EAST,x,y-1) `delete` todo)
                             | (NORTH,x,y) `elem` todo = 1+ compressSides (NORTH,x,y) ((NORTH,x,y) `delete` todo)
                             | (SOUTH,x+1,y-1) `elem` todo = 1+ compressSides (SOUTH,x+1,y-1) ((SOUTH,x+1,y-1) `delete` todo)
                             | otherwise = 1 + compressSides start sides'
    where
        start = head $ filter (\(s,xp,yp) -> s==NORTH && (s,xp+1,yp) `notElem` todo) todo
        sides' = start `delete` todo

compressSides (WEST,x,y) todo
                             | (WEST,x,y+1) `elem` todo = compressSides (WEST,x,y+1) ( (WEST,x,y+1) `delete` todo)
                             | (SOUTH,x,y) `elem` todo = 1+ compressSides (SOUTH,x,y) ((SOUTH,x,y) `delete` todo)
                             | (NORTH,x-1,y+1) `elem` todo = 1+ compressSides (NORTH,x-1,y+1) ((NORTH,x-1,y+1) `delete` todo)
                             | otherwise = 1 + compressSides start sides'
    where
        start = head $ filter (\(s,xp,yp) -> s==NORTH && (s,xp+1,yp) `notElem` todo) todo
        sides' = start `delete` todo

compress :: [(Dir, Int,Int)] -> Int
compress side = 1+ compressSides start sides'
    where
        start = head $ filter (\(s,x,y) -> s==NORTH && (s,x+1,y) `notElem` side) side
        sides' = start `delete` side

score' :: Int -> [[Char]] -> [(Char, S.Set (Int, Int))] -> Integer
score' len area elements = sum $ map (\(_,x) -> fromIntegral (S.size x) * fromIntegral (compress $ allSides len area x)) elements

part2 :: Int -> [[Char]] -> Integer
part2 len input= score' len input (partition len input input 0 0 S.empty [])

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    --f <- readFile "./test02.txt"
    let input = lines f
    let len = length $ head input
    print $ part1 len input
    print $ part2 len input
