module Main where
import Data.Set (Set, empty, insert, delete, member, toList, union, fromList, unions, (\\))
import qualified Data.Set as S

type Point = (Integer, Integer)

parse :: [String] -> (Set Point, Set Point, Point, String)
parse inp = (walls,boxes,robot,mov)
    where
        area = takeWhile (/= "") inp
        mov = filter (`elem` "<>^v" ) $ concat $  dropWhile (/= "") inp
        (walls,boxes,robot) = parseArea area

parseArea :: [String] -> (Set Point, Set Point, Point)
parseArea inp = dewit inp empty empty (0,0) 0 0
    where
        dewit [] walls boxes robot _ _ = (walls, boxes, robot)
        dewit ([]:ys) walls boxes robot _ y = dewit ys walls boxes robot 0 (y+1)
        dewit ((cur:xs):ys) walls boxes robot x y | cur == '#' = dewit (xs:ys) ((x,y) `insert` walls) boxes robot (x+1) y
                                                  | cur == 'O' = dewit (xs:ys) walls ((x,y) `insert` boxes) robot (x+1) y
                                                  | cur == '@' = dewit (xs:ys) walls boxes (x,y) (x+1) y
                                                  | otherwise = dewit (xs:ys) walls boxes robot (x+1) y

simulate :: Set Point -> Set Point -> Point -> Char -> (Set Point, Point)
simulate walls boxes (rx,ry) dir | dir == '>' && east == [] = (boxes,(rx+1,ry))
                                 | dir == '>' && fst (last east) == '#' = (boxes, (rx,ry))
                                 | dir == '>' = (eastBoxes, (rx+1,ry))
                                 | dir == '<' && west == [] = (boxes, (rx-1,ry))
                                 | dir == '<' && fst (last west) == '#' = (boxes, (rx,ry))
                                 | dir == '<' = (westBoxes, (rx-1,ry))
                                 | dir == 'v' && south == [] = (boxes, (rx,ry+1))
                                 | dir == 'v' && fst (last south) == '#' = (boxes, (rx,ry))
                                 | dir == 'v' = (southBoxes, (rx,ry+1))
                                 | dir == '^' && north == [] = (boxes, (rx,ry-1))
                                 | dir == '^' && fst (last north) == '#' = (boxes, (rx,ry))
                                 | dir == '^' = (northBoxes, (rx,ry-1))
                                 | otherwise = error "this should not happen"
    where
        east = checkEast (rx+1) ry
        eastBoxes = (fst (snd (last east))+1, snd (snd (last east)))`insert` (snd (head east) `delete` boxes)
        checkEast x y | (x,y) `member` boxes = ('O',(x,y)): checkEast (x+1) y
                      | (x,y) `member` walls = [('#',(x,y))]
                      | otherwise = []
        west = checkWest (rx-1) ry
        westBoxes = (fst (snd (last west))-1, snd (snd (last west)))`insert` (snd (head west) `delete` boxes)
        checkWest x y | (x,y) `member` boxes = ('O',(x,y)): checkWest (x-1) y
                      | (x,y) `member` walls = [('#',(x,y))]
                      | otherwise = []
        south = checkSouth rx (ry+1)
        southBoxes = (fst (snd (last south)), snd (snd (last south))+1)`insert` (snd (head south) `delete` boxes)
        checkSouth x y | (x,y) `member` boxes = ('O',(x,y)): checkSouth x (y+1)
                      | (x,y) `member` walls = [('#',(x,y))]
                      | otherwise = []
        north = checkNorth rx (ry-1)
        northBoxes = (fst (snd (last north)), snd (snd (last north))-1)`insert` (snd (head north) `delete` boxes)
        checkNorth x y | (x,y) `member` boxes = ('O',(x,y)): checkNorth x (y-1)
                      | (x,y) `member` walls = [('#',(x,y))]
                      | otherwise = []

simulateAll :: Set Point -> Set Point -> Point -> [Char] -> (Set Point, Point)
simulateAll _ boxes robot [] = (boxes, robot)
simulateAll walls boxes robot (c:cs) = simulateAll walls boxes' robot' cs
    where
        (boxes', robot') = simulate walls boxes robot c

gps :: Num a => (a, a) -> a
gps (rx,ry) = 100*ry + rx

showArea :: (Ord a, Ord b, Num a, Num b) => Set (a, b) -> Set (a, b) -> (a, b) -> a -> b -> a -> b -> [Char]
showArea walls boxes robot mx my x y | x >= mx && y >= my = []
                                     | x >= mx = '\n' : showArea walls boxes robot mx my 0 (y+1)
                                     | (x,y) == robot = '@' : showArea walls boxes robot mx my (x+1) y
                                     | (x,y) `member` walls = '#' : showArea walls boxes robot mx my (x+1) y
                                     | (x,y) `member` boxes = '[':']' : showArea walls boxes robot mx my (x+2) y
                                     | otherwise  = '.' : showArea walls boxes robot mx my (x+1) y

s :: Integer
s = 10

part1 :: Set Point -> Set Point -> Point -> [Char] -> Integer
part1 walls boxes robot movement = sum $ map gps (toList $ fst $ simulateAll walls boxes robot movement)

double :: Set Point -> Set Point -> Point -> (Set Point, Set Point, Point)
double walls boxes (rx,ry) = (walls', boxes', robot')
    where
        walls' = S.map (\(x,y) -> (x*2,y)) walls `union` S.map (\(x,y) -> (x*2+1,y)) walls
        boxes' = S.map (\(x,y) -> (x*2,y)) boxes
        robot' = (rx*2,ry)

collectNorth :: Set Point -> Set Point -> Point -> Set (Char, Point)
collectNorth walls boxes (x,y) = fromList $ boxes' ++ walls'
    where
        boxes' = [('O',(x-1,y-1))| (x-1,y-1) `elem` boxes] ++
                    [('O',(x,y-1))| (x,y-1) `elem` boxes] ++
                    [('O',(x+1,y-1))| (x+1,y-1) `elem` boxes]
        walls' = [('#',(x,y-1))| (x,y-1) `elem` walls] ++
                    [('#',(x+1,y-1))| (x+1,y-1) `elem` walls]

collectNorth' :: Set Point -> Set Point -> Set (Char, Point) -> Set (Char, Point)
collectNorth' walls boxes front | null front = empty
                                | any (\(c,_) -> c=='#') front = front
                                | otherwise = front `union` collectNorth' walls boxes front'
    where
        front' = unions $ map (collectNorth walls boxes . snd) (toList front)

collectSouth :: Set Point -> Set Point -> Point -> Set (Char, Point)
collectSouth walls boxes (x,y) = fromList $ boxes' ++ walls'
    where
        boxes' = [('O',(x-1,y+1))| (x-1,y+1) `elem` boxes] ++
                    [('O',(x,y+1))| (x,y+1) `elem` boxes] ++
                    [('O',(x+1,y+1))| (x+1,y+1) `elem` boxes]
        walls' = [('#',(x,y+1))| (x,y+1) `elem` walls] ++
                    [('#',(x+1,y+1))| (x+1,y+1) `elem` walls]

collectSouth' :: Set Point -> Set Point -> Set (Char, Point) -> Set (Char, Point)
collectSouth' walls boxes front | null front = empty
                                | any (\(c,_) -> c=='#') front = front
                                | otherwise = front `union` collectSouth' walls boxes front'
    where
        front' = unions $ map (collectSouth walls boxes . snd) (toList front)

collectEast :: Set Point -> Set Point -> (Char, Point) -> Set (Char, Point)
collectEast walls boxes (c,(x,y)) | c=='@' && (x+1,y) `member` walls = S.singleton ('#', (x+1,y))
                                  | c/='@' && (x+2,y) `member` walls = S.singleton ('#', (x+2,y))
                                  | c=='@' && (x+1,y) `member` boxes = ('O', (x+1,y)) `insert` collectEast walls boxes ('O', (x+1,y))
                                  | c/='@' && (x+2,y) `member` boxes = ('O', (x+2,y)) `insert` collectEast walls boxes ('O', (x+2,y))
                                  | otherwise = empty

collectWest :: Set Point -> Set Point -> (Char, Point) -> Set (Char, Point)
collectWest walls boxes (_,(x,y)) | (x-1,y) `member` walls = S.singleton ('#', (x-1,y))
                                  | (x-2,y) `member` boxes = ('O', (x-2,y)) `insert` collectWest walls boxes ('O', (x-2,y))
                                  | otherwise = empty

move:: Point -> Point -> Point
move (x,y) (dx,dy) = (x+dx, y+dy)

simulateWide :: Set Point -> Set Point -> Point -> Char -> (Set Point, Point)
simulateWide walls boxes (rx,ry) dir | dir == '>' && east == empty = (boxes,(rx+1,ry))
                                     | dir == '>' && any (\(c,_) -> c=='#') east = (boxes, (rx,ry))
                                     | dir == '>' = (eastBoxes, (rx+1,ry))
                                     | dir == '<' && west == empty = (boxes, (rx-1,ry))
                                     | dir == '<' && any (\(c,_) -> c=='#') west = (boxes, (rx,ry))
                                     | dir == '<' = (westBoxes, (rx-1,ry))
                                     | dir == 'v' && south == empty = (boxes, (rx,ry+1))
                                     | dir == 'v' && any (\(c,_) -> c=='#') south = (boxes, (rx,ry))
                                     | dir == 'v' = (southBoxes, (rx,ry+1))
                                     | dir == '^' && north == empty = (boxes, (rx,ry-1))
                                     | dir == '^' && any (\(c,_) -> c=='#') north = (boxes, (rx,ry))
                                     | dir == '^' = (northBoxes, (rx,ry-1))
                                     | otherwise = error "this should not happen"
    where
        east = collectEast walls boxes ('@',(rx,ry))
        eastBoxes = S.map (\(_,x) -> move x (1,0)) (S.filter (\(c,_) -> c=='O') east) `union` (boxes \\ S.map snd east)
        west = collectWest walls boxes ('@',(rx,ry))
        westBoxes = S.map (\(_,x) -> move x (-1,0)) (S.filter (\(c,_) -> c=='O') west) `union` (boxes \\ S.map snd west)
        south = collectSouth' walls boxes (fromList $ [('O',(rx-1,ry+1)) | (rx-1,ry+1) `member` boxes] ++
                                                        [('O',(rx,ry+1)) | (rx,ry+1) `member` boxes] ++
                                                        [('#',(rx,ry+1)) | (rx,ry+1) `member` walls])
        southBoxes = S.map (\(_,x) -> move x (0,1)) (S.filter (\(c,_) -> c=='O') south)  `union` (boxes \\ S.map snd south)
        north = collectNorth' walls boxes (fromList $ [('O',(rx-1,ry-1)) | (rx-1,ry-1) `member` boxes] ++
                                                        [('O',(rx,ry-1)) | (rx,ry-1) `member` boxes] ++
                                                        [('#',(rx,ry-1)) | (rx,ry-1) `member` walls])
        northBoxes = S.map (\(_,x) -> move x (0,-1)) (S.filter (\(c,_) -> c=='O') north) `union` (boxes \\ S.map snd north)

simulateAllWide :: Set Point -> Set Point -> Point -> [Char] -> (Set Point, Point)
simulateAllWide _ boxes robot [] = (boxes, robot)
simulateAllWide walls boxes robot (c:cs) = simulateAllWide walls boxes' robot' cs
    where
        (boxes', robot') = simulateWide walls boxes robot c

part2 :: Set Point -> Set Point -> Point -> String -> Integer
part2 walls boxes robot movement = sum $ map gps (toList boxes2)
    where
        (walls',boxes',robot') = double walls boxes robot
        (boxes2, _) = simulateAllWide walls' boxes' robot' movement

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (walls,boxes,robot, movement) = parse $ lines f
    print $ part1 walls boxes robot movement
    print $ part2 walls boxes robot movement
