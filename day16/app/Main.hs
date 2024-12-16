{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.Set (Set, empty, insert, notMember, findMin, fromList, union, delete)
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Debug.Trace (trace)

trace' :: Show a => a -> a
trace' a =  trace (show a) a

type Point = (Integer, Integer)

data Dir = NORTH | SOUTH | EAST | WEST
    deriving (Eq,Ord,Show)

next :: Set Point -> [(Integer, (Dir, Point))] -> [[(Integer, (Dir, Point))]]
next walls r@((dist, (NORTH, p@(x,y))):_) = [(dist+1,(NORTH, (x,y-1))):r | (x,y-1) `notMember` walls] ++
                                    [(dist+1000,(EAST, p)):r] ++ [(dist+1000,(WEST, p)):r]
next walls r@((dist, (SOUTH, p@(x,y))):_) = [(dist+1,(SOUTH, (x,y+1))):r | (x,y+1) `notMember` walls] ++
                                    [(dist+1000,(EAST, p)):r] ++ [(dist+1000,(WEST, p)):r]
next walls r@((dist, (WEST, p@(x,y))):_)  = [(dist+1,(WEST, (x-1,y))):r | (x-1,y) `notMember` walls] ++
                                    [(dist+1000,(NORTH, p)):r] ++ [(dist+1000,(SOUTH, p)):r]
next walls r@((dist, (EAST, p@(x,y))):_)  = [(dist+1,(EAST, (x+1,y))):r | (x+1,y) `notMember` walls] ++
                                    [(dist+1000,(NORTH, p)):r] ++ [(dist+1000,(SOUTH, p)):r]
next _ _ = error "this should not happen"

dijkstraPath :: (Set Point, Point) -> S.Set (Dir,Point) -> S.Set [(Integer,(Dir, Point))] -> [(Integer,(Dir, Point))]
dijkstraPath graph@(walls, e) visited q | p == e = min
                                    | i `S.member` visited = dijVis
                                    | otherwise = dijNew
    where
        min@((_, i@(_,p)):_) = findMin q
        n = next walls min
        dijVis = dijkstraPath graph visited (min `delete` q)
        dijNew = dijkstraPath graph (i `insert` visited) ((min `delete` q) `union` fromList n)

parse :: [String] -> Set Point -> Point -> Point -> Integer -> Integer -> (Set Point,Point,Point)
parse [] w s e  _ _ = (w, s, e)
parse ([]:ys) w s e _ y = parse ys w s e 0 (y+1)
parse ((cur:xs):ys) w s e x y | cur == 'S' = parse (xs:ys) w (x,y) e (x+1) y
                            | cur == 'E' = parse (xs:ys) w s (x,y) (x+1) y
                            | cur == '#' = parse (xs:ys) ((x,y) `insert` w) s e (x+1) y
                            | otherwise = parse (xs:ys) w s e (x+1) y

part1 :: Set Point -> Point -> Point -> Integer
part1 walls start end = fst $ head $ dijkstraPath (walls,end) empty (S.singleton [(0, (EAST, start))]) 

dim :: Integer
dim = 15

showArea :: Foldable t => Set (Integer, Integer) -> t (Integer, Integer) -> Integer -> Integer -> [Char]
showArea _ _ _ y | y == dim = ""
showArea walls path x y | x >= dim = '\n' : showArea walls path 0 (y+1)
                        | (x,y) `S.member` walls = '#' : showArea walls path (x+1) y
                        | (x,y) `elem` path = 'o' : showArea walls path (x+1) y
                        | otherwise = '.' : showArea walls path (x+1) y

next' :: Set Point -> (Integer, (Dir, Point)) -> [(Integer, (Dir, Point))]
next' walls (dist, (NORTH, p@(x,y))) = [(dist+1,(NORTH, (x,y-1))) | (x,y-1) `notMember` walls] ++
                                    [(dist+1000,(EAST, p))] ++ [(dist+1000,(WEST, p))]
next' walls (dist, (SOUTH, p@(x,y))) = [(dist+1,(SOUTH, (x,y+1))) | (x,y+1) `notMember` walls] ++
                                    [(dist+1000,(EAST, p))] ++ [(dist+1000,(WEST, p))]
next' walls (dist, (WEST, p@(x,y)))  = [(dist+1,(WEST, (x-1,y))) | (x-1,y) `notMember` walls] ++
                                    [(dist+1000,(NORTH, p))] ++ [(dist+1000,(SOUTH, p))]
next' walls (dist, (EAST, p@(x,y)))  = [(dist+1,(EAST, (x+1,y))) | (x+1,y) `notMember` walls] ++
                                    [(dist+1000,(NORTH, p))] ++ [(dist+1000,(SOUTH, p))]

dijkstraAnnotate :: Set Point -> Map (Dir,Point) (Integer,[(Dir,Point)]) -> S.Set (Integer,(Dir, Point)) -> S.Set (Dir, Point) -> Map (Dir,Point) (Integer,[(Dir,Point)])
dijkstraAnnotate walls annotations q done | null q = annotations
                                          | otherwise = dijkstraAnnotate walls annot''' q' done'
    where 
        min@(_, elem) = findMin q
        nexts = next' walls min
        nextsNew = M.fromList $ map (\(c,p)-> (p, (c,[elem]))) $ filter (\(_,p) -> p `M.notMember` annotations) nexts
        nextsLess = M.fromList $ map (\(c,p)-> (p, (c,[elem]))) $ filter (\(c,p) -> p `M.member` annotations && c < fst (annotations!p)) nexts
        nextsEq =  M.fromList $ map (\(c,p)-> (p, (c,[elem]))) $ filter (\(c,p) -> p `M.member` annotations && c == fst (annotations!p)) nexts
        annot' =  nextsNew `M.union` annotations
        annot'' = nextsLess `M.union` annot'
        annot''' = M.unionWith (\(c,l1)(_,l2) -> (c, l1 ++ l2)) nextsEq annot''
        done' = elem `insert` done
        q' = fromList (filter (\(_,p) -> p `S.notMember` done) nexts) `union` (min `delete` q)

dijkstraOnPaths :: Map (Dir,Point) (Integer,[(Dir,Point)]) -> Integer -> Point -> [(Dir,Point)]
dijkstraOnPaths annot target end = dewit ends
    where
        ends = [(NORTH,end) | fst (annot!(NORTH,end)) == target] ++
                [(SOUTH,end) | fst (annot!(SOUTH,end)) == target] ++
                [(EAST,end) | fst (annot!(EAST,end)) == target] ++
                [(WEST,end) | fst (annot!(EAST,end)) == target]
        dewit [] = []
        dewit front = front ++ dewit (concatMap (predecessors annot) front)

predecessors :: Map (Dir,Point) (Integer,[(Dir,Point)]) -> (Dir,Point) -> [(Dir, Point)]
predecessors annot cur = prev
    where
        prev = snd $ annot!cur


main :: IO ()
main = do
    f <- readFile "./test01.txt"
    let (walls, start, end) = parse (lines f) empty (0,0) (0,0) 0 0
    let target = part1 walls start end
    print target
    let annotations = dijkstraAnnotate walls M.empty (S.singleton (0, (EAST, start))) S.empty
    let list = dijkstraOnPaths annotations target end
    print list
    --putStrLn $ showArea walls path 0 0