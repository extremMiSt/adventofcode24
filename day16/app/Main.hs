module Main where

import Data.Set (Set, empty, insert, notMember, findMin, fromList, union, delete)
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List (nub)

type Point = (Integer, Integer)

data Dir = NORTH | SOUTH | EAST | WEST
    deriving (Eq,Ord,Show)

parse :: [String] -> Set Point -> Point -> Point -> Integer -> Integer -> (Set Point,Point,Point)
parse [] w s e  _ _ = (w, s, e)
parse ([]:ys) w s e _ y = parse ys w s e 0 (y+1)
parse ((cur:xs):ys) w s e x y | cur == 'S' = parse (xs:ys) w (x,y) e (x+1) y
                            | cur == 'E' = parse (xs:ys) w s (x,y) (x+1) y
                            | cur == '#' = parse (xs:ys) ((x,y) `insert` w) s e (x+1) y
                            | otherwise = parse (xs:ys) w s e (x+1) y

next :: Set Point -> (Integer, (Dir, Point)) -> [(Integer, (Dir, Point))]
next walls (dist, (NORTH, p@(x,y))) = [(dist+1,(NORTH, (x,y-1))) | (x,y-1) `notMember` walls] ++
                                    [(dist+1000,(EAST, p))] ++ [(dist+1000,(WEST, p))]
next walls (dist, (SOUTH, p@(x,y))) = [(dist+1,(SOUTH, (x,y+1))) | (x,y+1) `notMember` walls] ++
                                    [(dist+1000,(EAST, p))] ++ [(dist+1000,(WEST, p))]
next walls (dist, (WEST, p@(x,y)))  = [(dist+1,(WEST, (x-1,y))) | (x-1,y) `notMember` walls] ++
                                    [(dist+1000,(NORTH, p))] ++ [(dist+1000,(SOUTH, p))]
next walls (dist, (EAST, p@(x,y)))  = [(dist+1,(EAST, (x+1,y))) | (x+1,y) `notMember` walls] ++
                                    [(dist+1000,(NORTH, p))] ++ [(dist+1000,(SOUTH, p))]

dijkstraAnnotate :: Set Point -> Map (Dir,Point) (Integer,[(Dir,Point)]) -> Set (Integer,(Dir, Point)) -> Set (Dir, Point) -> Map (Dir,Point) (Integer,[(Dir,Point)])
dijkstraAnnotate walls annotations q done | null q = annotations
                                          | otherwise = dijkstraAnnotate walls annot' q' done'
    where 
        cur@(_, pos) = findMin q
        nexts = next walls cur
        nextsNew = M.fromList $ map (\(c,p)-> (p, (c,[pos]))) $ filter (\(_,p) -> p `M.notMember` annotations) nexts
        nextsLess = M.fromList $ map (\(c,p)-> (p, (c,[pos]))) $ filter (\(c,p) -> p `M.member` annotations && c < fst (annotations!p)) nexts
        nextsEq =  M.fromList $ map (\(c,p)-> (p, (c,[pos]))) $ filter (\(c,p) -> p `M.member` annotations && c == fst (annotations!p)) nexts
        annot' = M.unionWith (\(c,l1)(_,l2) -> (c, l1 ++ l2)) nextsEq (nextsLess `M.union` (nextsNew `M.union` annotations))
        done' = pos `insert` done
        q' = fromList (filter (\(_,p) -> p `S.notMember` done) nexts) `union` (cur `delete` q)

dijkstraOnShortestPaths :: Map (Dir,Point) (Integer,[(Dir,Point)]) -> Integer -> Point -> [(Dir,Point)]
dijkstraOnShortestPaths annot target end = dewit ends
    where
        ends = [(NORTH,end) | fst (annot!(NORTH,end)) == target] ++
                [(SOUTH,end) | fst (annot!(SOUTH,end)) == target] ++
                [(EAST,end) | fst (annot!(EAST,end)) == target] ++
                [(WEST,end) | fst (annot!(EAST,end)) == target]
        dewit [] = []
        dewit front = front ++ dewit (concatMap (predecessors annot) front)

predecessors :: Map (Dir,Point) (Integer,[(Dir,Point)]) -> (Dir,Point) -> [(Dir, Point)]
predecessors annot cur | fst (annot!cur) == 0 = []
                       | otherwise = prev
    where
        prev = snd $ annot!cur

part1 ::Map (Dir, Point) (Integer, [(Dir, Point)]) -> Point -> Integer
part1 annotations end = minimum ends
    where
        ends = [fst (annotations!(NORTH,end)), fst (annotations!(SOUTH,end)), fst (annotations!(EAST,end)), fst (annotations!(EAST,end))]

part2 :: Map (Dir, Point) (Integer, [(Dir, Point)]) -> Point -> Integer -> Int
part2 annotations end target = length $ nub $ map snd list
    where
        list = dijkstraOnShortestPaths annotations target end

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (walls, start, end) = parse (lines f) empty (0,0) (0,0) 0 0
    let annotations = dijkstraAnnotate walls (M.singleton (EAST, start) (0,[])) (S.singleton (0, (EAST, start))) S.empty
    let target = part1 annotations end
    print target
    print $ part2 annotations end target