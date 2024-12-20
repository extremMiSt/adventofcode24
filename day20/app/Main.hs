module Main where
import Data.Set (Set, findMin, insert, fromList, union, delete, notMember)
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M

type Point = (Integer, Integer)

parse :: [String] -> Set Point -> Set Point-> Point -> Point -> Integer -> Integer -> (Set Point,Set Point,Point,Point)
parse [] w p s e  _ _ = (w, p, s, e)
parse ([]:ys) w p s e _ y = parse ys w p s e 0 (y+1)
parse ((cur:xs):ys) w p s e x y | cur == 'S' = parse (xs:ys) w ((x,y) `insert` p) (x,y) e (x+1) y
                            | cur == 'E' = parse (xs:ys) w ((x,y) `insert` p) s (x,y) (x+1) y
                            | cur == '#' = parse (xs:ys) ((x,y) `insert` w) p s e (x+1) y
                            | otherwise = parse (xs:ys) w ((x,y) `insert` p) s e (x+1) y

next :: Set Point -> (Integer, Point) -> [(Integer, Point)]
next walls (dist, (x,y)) = north ++ south ++ west ++ east
    where
        north = [(dist+1, (x,y-1)) | (x,y-1) `notMember` walls]
        south = [(dist+1, (x,y+1)) | (x,y+1) `notMember` walls]
        west = [(dist+1, (x-1,y)) | (x-1,y) `notMember` walls]
        east = [(dist+1, (x+1,y)) | (x+1,y) `notMember` walls]

dijkstraAnnotate :: Set Point -> Map Point (Integer,Point) -> Set (Integer, Point) -> Set Point -> Map Point (Integer,Point)
dijkstraAnnotate walls annotations q done | null q = annotations
                                          | otherwise = dijkstraAnnotate walls annot' q' done'
    where
        cur@(_, pos) = findMin q
        nexts = next walls cur
        nextsNew = M.fromList $ map (\(c,p)-> (p, (c,pos))) $ filter (\(_,p) -> p `M.notMember` annotations) nexts
        nextsLess = M.fromList $ map (\(c,p)-> (p, (c,pos))) $ filter (\(c,p) -> p `M.member` annotations && c < fst (annotations!p)) nexts
        nextsEq =  M.fromList $ map (\(c,p)-> (p, (c,pos))) $ filter (\(c,p) -> p `M.member` annotations && c == fst (annotations!p)) nexts
        annot' = M.unionWith (\(c,l1)(_,_) -> (c, l1)) nextsEq (nextsLess `M.union` (nextsNew `M.union` annotations))
        done' = pos `insert` done
        q' = fromList (filter (\(_,p) -> p `S.notMember` done) nexts) `union` (cur `delete` q)

taxicab :: Point -> Point -> Integer
taxicab (x1,y1) (x2,y2) = abs (x1-x2)+abs (y1-y2)

shortcuts :: Integer -> Map Point (Integer, Point) -> [Point] -> Int
shortcuts len annot paths = length $ filter (\(p1,p2) -> (fst (annot!p1) - fst (annot!p2) - taxicab p1 p2) >= 100)  candidates
    where 
        candidates = [(p1,p2) | p1 <- paths, p2<-paths, p1 /= p2 && taxicab p1 p2 <= len]

part1 :: Map Point (Integer, Point) -> [Point] -> Int
part1 = shortcuts 2

part2 :: Map Point (Integer, Point) -> [Point] -> Int
part2 = shortcuts 20

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (walls, path, _, end) = parse (lines f) S.empty S.empty (0,0) (0,0) 0 0
    let annot = dijkstraAnnotate walls (M.singleton end (0,end)) (S.singleton (0, end)) S.empty
    print $ part1 annot (S.toList path)
    print $ part2 annot (S.toList path)
    return ()
