{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (><))
import qualified Data.Set as S
import qualified Data.Foldable as F

class Ord a => Graph b a where
    neighbours :: b -> a -> [a]

reacheableBFS :: Graph b a => b -> S.Set a -> Seq a -> S.Set a
reacheableBFS _ visited Q.Empty = visited
reacheableBFS graph visited (e:<|q) | e `elem` visited = reacheableBFS graph visited q
                                    | otherwise = reacheableBFS graph (S.insert e visited) (q >< Q.fromList (neighbours graph e))


instance Graph  [[Int]] (Int,Int) where
    neighbours :: [[Int]] -> (Int,Int) -> [(Int,Int)]
    neighbours area (xp,yp) = north ++ south ++ east ++ west
        where
            north = [(xp-1,yp) | (xp-1) >= 0 && area!!yp!!(xp-1) == 1+area!!yp!!xp]
            south = [(xp+1,yp) | (xp+1) < length (head area) && area!!yp!!(xp+1) == 1+area!!yp!!xp]
            east = [(xp,yp-1) | (yp-1) >= 0 && area!!(yp-1)!!xp == 1+area!!yp!!xp]
            west = [(xp,yp+1) | (yp+1) < length (head area) && area!!(yp+1)!!xp == 1+area!!yp!!xp]

starts :: [[Int]] -> [(Int,Int)]
starts area = dewit area 0 0
    where
        dewit ((x:xs):ys) xp yp | x == 0 = (xp,yp) : dewit (xs:ys) (xp+1) yp
                                | otherwise = dewit (xs:ys) (xp+1) yp
        dewit ([]:ys) _ yp = dewit ys 0 (yp+1)
        dewit [] _ _ = []

score :: [[Int]] -> (Int,Int) -> Int
score area start = length nines
    where
        reachable = reacheableBFS area S.empty (Q.fromList [start])
        nines = filter (\(x,y) -> area!!y!!x ==9) (F.toList reachable)

part1 :: [[Int]] -> [(Int, Int)] -> Int
part1 area start = sum $ map (score area) start

instance Graph  [[Int]] [(Int,Int)] where
    neighbours :: [[Int]] -> [(Int,Int)] -> [[(Int,Int)]]
    neighbours area path@((xp,yp):_) = north ++ south ++ east ++ west
        where
            north = [(xp-1,yp):path | (xp-1) >= 0 && area!!yp!!(xp-1) == 1+area!!yp!!xp]
            south = [(xp+1,yp):path | (xp+1) < length (head area) && area!!yp!!(xp+1) == 1+area!!yp!!xp]
            east = [(xp,yp-1):path | (yp-1) >= 0 && area!!(yp-1)!!xp == 1+area!!yp!!xp]
            west = [(xp,yp+1):path | (yp+1) < length (head area) && area!!(yp+1)!!xp == 1+area!!yp!!xp]
    neighbours _ _ = error "this should not happen"

part2 :: [[Int]] -> [(Int, Int)] -> Int
part2 area start = sum $ map (rating area) start

rating :: [[Int]] -> (Int,Int) -> Int
rating area start = length nines
    where
        reachable = reacheableBFS area S.empty (Q.fromList [[start]])
        nines = filter (\((x,y):_) -> area!!y!!x ==9) (F.toList reachable)

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let area = map (map (\x -> read [x])) (lines f)
    let start = starts area
    print $ part1 area start
    print $ part2 area start
