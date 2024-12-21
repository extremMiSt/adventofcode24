module Main where
import Data.List (nub, genericLength)

type Point = (Int,Int)
type Pad = [[String]]
type Robot = (Point, Pad)

{-
numpad :: Pad
numpad = [["7","8","9"],["4","5","6"],["1","2","3"],["X","0","A"]]

dpad :: Pad
dpad = [["X","^","A"],["<","v",">"]]

over :: Robot -> String
over ((x,y),pad)    | y < 0 || y >= length pad || x < 0 || x >= length (pad!!y) = error "panic!"
                    | ov == "X" = error "panic!"
                    | otherwise = ov
    where
        ov = pad!!y!!x

robotNum :: Robot
robotNum = ((2,3), numpad)

robotDir :: Robot
robotDir = ((2,0), dpad)

inputC :: String -> Robot -> (Robot, String)
inputC "<" ((x,y), pad) = (((x-1,y),pad), "")
inputC ">" ((x,y), pad) = (((x+1,y),pad), "")
inputC "^" ((x,y), pad) = (((x,y-1),pad), "")
inputC "v" ((x,y), pad) = (((x,y+1),pad), "")
inputC "A" ((x,y), pad) = (((x,y),pad), over ((x,y),pad))
inputC _ _ = error "what?"

input :: [Char] -> Robot -> String
input [] _ = []
input (x:xs) robot = out ++  input xs r'
    where
        (r', out) = inputC [x] robot

example :: [Char] -> [Char]
example str = line ++ "\n" ++ line' ++ "\n" ++ line'' ++ "\n" ++ line'''
    where
        line = str
        line' = input line robotDir
        line'' = input line' robotDir
        line''' = input line'' robotNum -}


numCoord :: Char -> Point
numCoord 'A' = (2,3)
numCoord '0' = (1,3)
numCoord '1' = (0,2)
numCoord '2' = (1,2)
numCoord '3' = (2,2)
numCoord '4' = (0,1)
numCoord '5' = (1,1)
numCoord '6' = (2,1)
numCoord '7' = (0,0)
numCoord '8' = (1,0)
numCoord '9' = (2,0)
numCoord _ = error "how?"

mapping :: Char -> Char -> String
mapping 'A' '<' = "v<<A"
mapping 'A' '>' = "vA"
mapping 'A' '^' = "<A"
mapping 'A' 'v' = "<vA"

mapping '<' 'A' = ">>^A"
mapping '<' '>' = error "why?"
mapping '<' '^' = ">^A"
mapping '<' 'v' = ">A"

mapping '>' 'A' = "^A"
mapping '>' '<' = error "why?"
mapping '>' '^' = "<^A"
mapping '>' 'v' = "<A"

mapping '^' 'A' = ">A"
mapping '^' '>' = "v>A" -- or >vA
mapping '^' '<' = "v<A"
mapping '^' 'v' = error "why?"

mapping 'v' 'A' = ">^A"
mapping 'v' '<' = "<A"
mapping 'v' '>' = ">A"
mapping 'v' '^' = error "why?"

mapping a b | a==b = "A"
mapping a b = error ("dang " ++  [a] ++ " " ++ [b] ++"!")

mapper :: Char -> String -> String
mapper _ [] = []
mapper c (x:xs) = mapping c x ++ mapper x xs

numPaths :: Char -> Char -> [String]
numPaths start goal= nub $ xFirst ++ yFirst
    where
        (sx, sy) = numCoord start
        (gx, gy) = numCoord goal
        xDiff = sx - gx
        yDiff = sy - gy
        xDir = replicate (abs xDiff) (if xDiff >= 0 then '<' else '>')
        yDir = replicate (abs yDiff) (if yDiff >= 0 then '^' else 'v')
        xFirst = [xDir ++ yDir | sy /= 3 || gx /= 0]
        yFirst = [yDir ++ xDir | sx /= 0 || gy /= 3]

paths :: [[Char]] -> Char -> [Char] -> [[Char]]
paths prefix _ [] = prefix
paths prefix cur (a:as) = paths (concatMap (\x -> [x++p++"A" | p<- numPaths cur a]) prefix) a as

score :: String -> Integer
score str = read (take 3 str) * minimum (map (genericLength . mapper 'A' . mapper 'A') (paths [""] 'A' str))

part1 :: String -> Integer
part1 f = sum $ map score $ lines f

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    print $ part1 f
