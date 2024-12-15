module Main where
import Data.Set (Set, empty, insert, delete, member)

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
        checkNorth x y | (x,y) `member` boxes = ('O',(x,y)): checkSouth x (y-1)  
                      | (x,y) `member` walls = [('#',(x,y))]
                      | otherwise = []

showArea walls boxes robot mx my x y | x > mx && y >= my = []
                                     | x > mx = '\n' : showArea walls boxes robot mx my 0 (y+1)
                                     | (x,y) == robot = '@' : showArea walls boxes robot mx my (x+1) y
                                     | (x,y) `member` walls = '#' : showArea walls boxes robot mx my (x+1) y
                                     | (x,y) `member` boxes = 'O' : showArea walls boxes robot mx my (x+1) y
                                     | otherwise  = '.' : showArea walls boxes robot mx my (x+1) y


main :: IO ()
main = do
    f <- readFile "./example01.txt"
    let (walls,boxes,robot, movement) = parse $ lines f
    putStrLn $ showArea walls boxes robot 7 7 0 0
    let (boxes1,robot1) = simulate walls boxes robot (head movement)
    putStrLn $ showArea walls boxes1 robot1 7 7 0 0


