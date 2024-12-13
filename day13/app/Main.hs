module Main where

type Point = (Integer, Integer)
type Machine = (Point,Point,Point)

parseMachine :: [String] -> Machine
parseMachine [aLine, bLine, targetLine] = ((ax, ay),(bx,by),(tx,ty))
    where
        aWords = words aLine
        bWords = words bLine
        tWords = words targetLine
        ax = read $ init $ drop 2 $ aWords!!2
        ay = read $ drop 2 $ aWords!!3
        bx = read $ init $ drop 2 $ bWords!!2
        by = read $ drop 2 $ bWords!!3
        tx = read $ init $ drop 2 $ tWords!!1
        ty = read $ drop 2 $ tWords!!2
parseMachine _ = error "this should not happen"

parse :: [String] -> [Machine]
parse ("":rs) = parse rs
parse (a:b:t:rs) = parseMachine [a,b,t] : parse rs
parse [] = []
parse _ = error "this should not happen"

-- a1*x + b1*y + c = 0
-- ax*a + bx*b - tx = 0
-- ay*a + by*b - ty = 0

solveMachine :: Machine -> Point
solveMachine ((ax, ay),(bx,by),(tx,ty)) | ax*by - ay*bx == 0 = (0,0)
                                        | a > 100 || b > 100 = (0,0)
                                        | ax*a + bx*b /= tx = (0,0)
                                        | ay*a + by*b /= ty = (0,0)
                                        | otherwise = (a,b)
    where 
        a = (bx*(-ty) - by*(-tx)) `div` (ax*by - ay*bx) 
        b = ((-tx)*ay - (-ty)*ax) `div` (ax*by - ay*bx)

score :: Num a => (a, a) -> a
score (a,b) = a*3+b

part1 :: [Machine] -> Integer
part1 machines = sum $ map (score. solveMachine) machines

solveMachine' :: Machine -> Point
solveMachine' ((ax, ay),(bx,by),(tx,ty)) | ax*by - ay*bx == 0 = (0,0)
                                        | ax*a + bx*b /= tx' = (0,0)
                                        | ay*a + by*b /= ty' = (0,0)
                                        | otherwise = (a,b)
    where 
        tx' = 10000000000000 + tx
        ty' = 10000000000000 + ty
        a = (bx*(-ty') - by*(-tx')) `div` (ax*by - ay*bx) 
        b = ((-tx')*ay - (-ty')*ax) `div` (ax*by - ay*bx)

part2 :: [Machine] -> Integer
part2 machines = sum $ map (score. solveMachine') machines

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let machines = parse $ lines f
    print $ part1 machines
    print $ part2 machines
    return ()