{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import qualified Data.Set as S
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Q
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

trace' :: Show a => a -> a
trace' a = trace (show a) a

type Point = (Integer, Integer)

class Ord a => Graph b a where
    neighbours :: b -> a -> [a]

reacheableBFS :: Graph b a => b -> S.Set a -> Seq a -> S.Set a
reacheableBFS _ visited Q.Empty = visited
reacheableBFS graph visited (e:<|q) | e `elem` visited = reacheableBFS graph visited q
                                    | otherwise = reacheableBFS graph (S.insert e visited) (q >< Q.fromList (neighbours graph e))

-- type where a is annotated with b
data An a b = An a b
    deriving (Show)

load :: An a b -> a
load (An a _) = a

annot :: An a b -> b
annot (An _ b) = b

instance Eq a => Eq (An a b) where
    (==) :: Eq a => An a b -> An a b -> Bool
    An a _ == An c _ = a == c

instance Ord a => Ord (An a b) where
  compare :: Ord a => An a b -> An a b -> Ordering
  compare (An a _) (An c _) = compare a c

pathBFS :: Graph b a => b -> a -> S.Set (An a a) -> Q.Seq (An a a) -> S.Set (An a a)
pathBFS _ _ _ Q.Empty = S.empty
pathBFS graph goal visited (e:<|q) | e `S.member` visited = pathBFS graph goal visited q
                                   | e == An goal goal = S.insert e visited
                                   | otherwise = pathBFS graph goal (S.insert e visited) (q >< Q.fromList (map (\x -> An x (load e)) (neighbours graph (load e))))

getPath :: (Eq a, Ord a) => S.Set (An a a) -> a -> a -> [a]
getPath set element stop | An element element `S.notMember` set = []
                      | element == annot elmnt = []
                      | otherwise = element : getPath set (annot elmnt) stop
                        where
                            elmnt = fromJust (S.lookupLE (An element element) set)

path :: Graph b a => b -> a -> a -> [a]
path graph start end = p
    where
        p = getPath set end start
        set = pathBFS graph end S.empty (Q.singleton (An start start))

instance Graph  (S.Set Point) Point where
    neighbours :: S.Set Point -> Point -> [Point]
    neighbours area (xp,yp) = north ++ south ++ east ++ west
        where
            north = [(xp,yp-1) | yp-1 >= 0 && ((xp,yp-1) `S.notMember` area)]
            south = [(xp,yp+1) | yp+1 <= s && ((xp,yp+1) `S.notMember` area)]
            west = [(xp-1,yp) | xp-1 >= 0 && ((xp-1,yp) `S.notMember` area)]
            east = [(xp+1,yp) | xp+1 <= s && ((xp+1,yp) `S.notMember` area)]

parse :: String -> Point
parse str = (read $ head split, read $ split!!1)
    where
        split = splitOn ","  str

showArea :: (Foldable t1, Foldable t2) => t2 (Integer, Integer) -> t1 (Integer, Integer) -> [Char]
showArea area p = dewit 0 0
    where
        dewit x y   | y > s = []
                    | x > s = '\n' : dewit 0 (y+1)
                    | (x,y) `elem` p = 'o' : dewit (x+1) y
                    | (x,y) `elem` area = '#' : dewit (x+1) y
                    | otherwise = '.'  : dewit (x+1) y

part1 :: (Graph (S.Set a) (Integer, Integer), Ord a) => [a] -> Int
part1 bytes = length $ path kb (z,z) (70,70)
    where
        kb = S.fromList $ take 1024 bytes

part2 :: [Point] -> IO Point
part2 bytes = dewit 1024
    where
        dewit :: Int -> IO Point
        dewit n = do
            let prefix = take n bytes
            let notReacheable = null (path (S.fromList prefix) (z,z) (70,70))
            if notReacheable then
                return $ last prefix
            else
                dewit (n+1)

s :: Integer
s = 70

z :: Integer
z = 0

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let bytes = map parse $ lines f
    print $ part1 bytes
    p <- part2 bytes
    print p
    return ()
