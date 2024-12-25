module Main where
import Numeric (readBin)
import Data.Bits ((.&.))
import Data.List.Split (splitOn)

parse :: [Char] -> [Integer]
parse f = map (fst . head . readBin . bin) things
    where
        things = map concat (splitOn [""] (lines f))
        bin [] = []
        bin ('#':xs) = '1' : bin xs
        bin ('.':xs) = '0' : bin xs
        bin _ = error "this should not happen"

part1 :: [Integer] -> Int
part1 f = length (filter (==0) [a .&. b | a <- f, b <- f]) `div` 2

main :: IO ()
main = do
    f <- parse <$> readFile "./input01.txt"
    print $ part1 f
