module Main where
import Data.Set (Set, empty, union, unions)
import qualified Data.Set as Set

s :: Integer
s = 50

beaconTypes :: [Char]
beaconTypes = ['0'..'9']++['a'..'z']++['A'..'Z']

parse :: [String] -> [(Char, Integer, Integer)]
parse file = dewit file 0 0
    where 
        dewit ((x:xs):y) xp yp  | x /= '.' = (x, xp, yp) : dewit (xs:y) (xp+1) yp 
                                | otherwise = dewit (xs:y) (xp+1) yp 
        dewit ([]:y) _ yp = dewit y 0 (yp+1) 
        dewit [] _ _ = []

inBounds :: (Integer, Integer) -> Bool
inBounds (x,y) = x>=0 && y>= 0 && x<s && y<s

antinodePair :: (a1, Integer, Integer)-> (a2, Integer, Integer) -> Set (Integer, Integer)
antinodePair (_,x1,y1) (_,x2,y2) = Set.fromList $ filter inBounds [(x1-diffX,y1-diffY), (x2+diffX,y2+diffY)]
    where
        diffX = x2-x1
        diffY = y2-y1

antinodeLine :: (a1, Integer, Integer)-> (a2, Integer, Integer) -> Set (Integer, Integer)
antinodeLine (_,x1,y1) (_,x2,y2) = Set.fromList $ 
                        takeWhile inBounds (rep (x2,y2) (-diffX,-diffY)) ++ 
                        takeWhile inBounds (rep (x2,y2) (diffX,diffY))
    where
        diffX = x2-x1
        diffY = y2-y1

rep :: (Num a, Num b) => (a, b) -> (a, b) -> [(a, b)]
rep (a,b) (dx,dy) = (a,b) : rep (a+dx, b+dy) (dx,dy)

antinodes :: [(a2, Integer, Integer)] -> Set (Integer, Integer)
antinodes (a:r) = unions (map (antinodePair a) r)  `union` antinodes r
antinodes [] = empty

antinodes' :: [(a2, Integer, Integer)] -> Set (Integer, Integer)
antinodes' (a:r) = unions (map (antinodeLine a) r)  `union` antinodes' r
antinodes' [] = empty

ofType :: Eq a => [(a, b, c)] -> a -> [(a, b, c)]
ofType beacons typ = filter (\(n,_,_) -> n==typ) beacons

part1 :: [(Char, Integer, Integer)] -> Int
part1 beacons = Set.size $ unions $ map (antinodes . ofType beacons) beaconTypes

part2 :: [(Char, Integer, Integer)] -> Int
part2 beacons = Set.size $ unions $ map (antinodes' . ofType beacons) beaconTypes

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let beacons = parse $ lines f
    print $ part1 beacons
    print $ part2 beacons

