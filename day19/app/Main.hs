module Main where
import Data.List.Split (splitOn)
import Data.Set (fromList,Set, member, toList, unions, singleton)
import qualified Data.Set as S
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

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (available, todo) = parse f
    let poss = part1 available todo
    print poss

