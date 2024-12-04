module Main where

maxV :: Int
maxV = 140

count :: Num b => [Bool] -> b
count xs = fromIntegral $ length (filter id xs)

findWest :: [[Char]] -> Int -> Int -> Bool
findWest ff x y = x < (140-3) && (ff!!y!!x == 'X' && ff!!y!!(x+1) == 'M' && ff!!y!!(x+2) == 'A' && ff!!y!!(x+3) == 'S')

findEast :: [[Char]] -> Int -> Int -> Bool
findEast ff x y = x >= 3 && (ff!!y!!x == 'X' && ff!!y!!(x-1) == 'M' && ff!!y!!(x-2) == 'A' && ff!!y!!(x-3) == 'S')

findSouth :: [[Char]] -> Int -> Int -> Bool
findSouth ff x y = y < (140-3) && (ff!!y!!x == 'X' && ff!!(y+1)!!x == 'M' && ff!!(y+2)!!x == 'A' && ff!!(y+3)!!x == 'S')

findNorth :: [[Char]] -> Int -> Int -> Bool
findNorth ff x y = y >= 3 && (ff!!y!!x == 'X' && ff!!(y-1)!!x == 'M' && ff!!(y-2)!!x == 'A' && ff!!(y-3)!!x == 'S')

findSouthWest :: [[Char]] -> Int -> Int -> Bool
findSouthWest ff x y = (x < (140-3) && y < (140-3)) && (ff!!y!!x == 'X' && ff!!(y+1)!!(x+1) == 'M' && ff!!(y+2)!!(x+2) == 'A' && ff!!(y+3)!!(x+3) == 'S')

findSouthEast :: [[Char]] -> Int -> Int -> Bool
findSouthEast ff x y = (x >= 3 && y < (140 - 3)) && (ff!!y!!x == 'X' && ff!!(y+1)!!(x-1) == 'M' && ff!!(y+2)!!(x-2) == 'A' && ff!!(y+3)!!(x-3) == 'S')

findNorthWest :: [[Char]] -> Int -> Int -> Bool
findNorthWest ff x y = (x < (140-3) && y >= 3) && (ff!!y!!x == 'X' && ff!!(y-1)!!(x+1) == 'M' && ff!!(y-2)!!(x+2) == 'A' && ff!!(y-3)!!(x+3) == 'S')

findNorthEast :: [[Char]] -> Int -> Int -> Bool
findNorthEast ff x y = (x >= 3 && y >= 3) && (ff!!y!!x == 'X' && ff!!(y-1)!!(x-1) == 'M' && ff!!(y-2)!!(x-2) == 'A' && ff!!(y-3)!!(x-3) == 'S')

countAt :: Num b => [[Char]] -> Int -> Int -> b
countAt ff x y = count [findWest ff x y, findEast ff x y, findSouth ff x y, findNorth ff x y,
                        findSouthWest ff x y, findSouthEast ff x y, findNorthWest ff x y, findNorthEast ff x y]

findX :: [[Char]] -> Int -> Int -> Bool
findX ff x y = x > 0 && y > 0 && x < (maxV-1) && y < (maxV-1) &&
                ff!!y!!x == 'A' && 
                ((ff!!(y+1)!!(x+1)=='M' && ff!!(y-1)!!(x-1)=='S') || (ff!!(y-1)!!(x-1)=='M' && ff!!(y+1)!!(x+1)=='S')) &&
                ((ff!!(y-1)!!(x+1)=='M' && ff!!(y+1)!!(x-1)=='S') || (ff!!(y+1)!!(x-1)=='M' && ff!!(y-1)!!(x+1)=='S'))


countTotal :: [[Char]] -> [[a2]] -> Int -> Int -> Int
countTotal ff ((_:as):bs) x y = countAt ff x y + countTotal ff (as:bs) (x+1) y
countTotal ff ([]:bs) _ y = countTotal ff bs 0 (y+1)
countTotal _ [] _ _ = 0

int :: Num a => Bool -> a
int b = if b then 1 else 0

countTotal' :: [[Char]] -> [[a2]] -> Int -> Int -> Int
countTotal' ff ((_:as):bs) x y = int (findX ff x y) + countTotal' ff (as:bs) (x+1) y
countTotal' ff ([]:bs) _ y = countTotal' ff bs 0 (y+1)
countTotal' _ [] _ _ = 0

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let ff = lines f
    print $ countTotal ff ff 0 0
    print $ countTotal' ff ff 0 0
