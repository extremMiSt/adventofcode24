module Main where
import Data.List.Split (splitOn)
import Data.Set (fromList,Set, member, toList, unions, singleton)
import qualified Data.Set as S
import Data.Map (Map, (!), delete)
import qualified Data.Map as M
import Data.List (isPrefixOf)

parse :: [Char] -> (Set [Char], [String])
parse f = (available, todo)
    where
        line = lines f
        available = fromList $ splitOn ", " (head line)
        todo = drop 2 line

matches :: Set String -> String -> Int-> Set Int
matches available arrangement  prev = shortened
    where
        match = S.filter (`isPrefixOf` drop prev arrangement) available
        shortened = S.map (\m -> prev + length m) match

possible :: Set String -> String -> Set Int -> Bool
possible avail todo pref | length todo `member` pref = True
                          | null nextMatches = False
                          | otherwise = possible avail todo nextMatches
    where
        nextMatches = unions $ map (matches avail todo) (toList pref)

part1 :: Set String -> [[Char]] -> Int
part1 available todo= length $ filter (\x -> possible available x (singleton 0)) todo

matches' :: Set String -> String -> Int -> Integer -> Map Int Integer
matches' available arrangement  prev num = shortened
    where
        match = S.filter (`isPrefixOf` drop prev arrangement) available
        shortened = M.fromList $ map (\m -> (prev + length m, num)) (toList match)

possibleArr :: Set String -> String -> Map Int Integer -> Integer
possibleArr avail todo pref | null pref = 0
                            | len `M.member` pref = pref!len + possibleArr avail todo (nextMatches $ len `delete` pref)
                            | otherwise = possibleArr avail todo (nextMatches pref)
    where
        len = length todo
        nextMatches pref' = M.unionsWith (+) $ map (uncurry (matches' avail todo)) (M.toList pref')

part2 :: Set String -> [String] -> Integer
part2 available todo = sum $ map (\x -> possibleArr available x (M.singleton 0 1)) todo

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (available, todo) = parse f
    print $ part1 available todo
    print $ part2 available todo


