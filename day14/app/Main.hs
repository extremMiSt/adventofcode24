module Main where
import Data.List.Split (splitOn)
import Data.Set (fromList, member)
import Control.Monad (when)
import Data.List (isInfixOf)

type Point = (Integer,Integer)


parseRobot :: String -> (Point,Point)
parseRobot s = ((read $ head ps, read $ ps!!1),(read $ head vs, read $ vs!!1))
    where
        word = words s
        p = drop 2 $ head word
        v = drop 2 $ word!!1
        ps = splitOn "," p
        vs = splitOn "," v

move :: Integer -> Integer -> Integer ->(Point,Point) ->  Point
move n mx my ((px,py),(vx, vy)) = (x,y)
    where
        x = (px + n*vx) `mod` mx
        y = (py + n*vy) `mod` my

renderRobots :: Integer -> Integer -> [Point] -> String
renderRobots mx my robots = render 0 0
    where
        s = fromList robots
        render x y | x >= mx && y >= my = []
                   | x >= mx = '\n' : render 0 (y+1)
                   | (x,y) `member` s = '#' : render (x+1) y
                   | otherwise = ' ' : render (x+1) y

part1 :: [(Point, Point)] -> Integer -> Integer -> Int
part1 s mx my = product $ map length [q1,q2,q3,q4]
    where
        robots = map (move 100 mx my) s
        q1 = filter (\(xp,yp) -> xp < mx`div`2 && yp < my`div`2) robots
        q2 = filter (\(xp,yp) -> xp > mx`div`2 && yp < my`div`2) robots
        q3 = filter (\(xp,yp) -> xp < mx`div`2 && yp > my`div`2) robots
        q4 = filter (\(xp,yp) -> xp > mx`div`2 && yp > my`div`2) robots

part2 :: [(Point, Point)] -> Integer -> Integer-> IO ()
part2 s mx my = loop 0
    where
        loop n = do
            appendFile "./out.txt" $ "\n\niteration: " ++ show n ++"\n"
            appendFile "./out.txt" $ renderRobots mx my (map (move n mx my) s)
            when (n < 10000) $ loop (n+1)

part2' :: [(Point, Point)] -> Integer -> Integer-> IO ()
part2' s mx my = loop 0
    where
        loop n = do
            let cur = renderRobots mx my (map (move n mx my) s)
            if "###############################" `isInfixOf` cur then do
                putStrLn $ "iteration: " ++ show n
                putStrLn cur
            else
                loop (n+1)

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let maxX = 101
    let maxY = 103
    --f <- readFile "./test01.txt"
    --let maxX = 11
    --let maxY = 7

    let robots = map parseRobot $ lines f
    print $ part1 robots maxX maxY
    --part2 robots maxX maxY -- this generates a 100mb file called out.txt
    part2' robots maxX maxY
