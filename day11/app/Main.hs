{-# LANGUAGE TupleSections #-}
module Main where
import Data.List (genericLength)
import Data.Map (Map)
import qualified Data.Map as M

step :: [Integer] -> [Integer]
step (0:ll) = 1 : step ll
step (l:ll) | even len = left : right : step ll
            | otherwise = l*2024 : step ll
    where
        left = read $ take (len `div` 2) str
        right = read $ drop (len `div` 2) str
        str = show l
        len = length str
step [] = []

task1 :: [Integer] -> Integer
task1 list = genericLength $ rep step 25 list

step' :: (Integer, Integer) -> Map Integer Integer
step' (0,n) = M.singleton 1 n
step' (l,n) | even len = M.unionWith (+) (M.singleton left n) (M.singleton right n)
            | otherwise = M.singleton (l*2024) n
    where
        left = read $ take (len `div` 2) str
        right = read $ drop (len `div` 2) str
        str = show l
        len = length str

stepMap :: Map Integer Integer -> Map Integer Integer
stepMap m = M.unionsWith (+) $ map step' (M.toList m)

rep :: (a -> a) -> Integer -> a -> a
rep _ 0 a = a
rep f n a = rep f (n-1) (f a)


task2 :: [Integer] -> Integer
task2 line = sum $ map snd $ M.toList $ rep stepMap 75 $  M.fromList $ map (, 1) line

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let line = map read $ words $ head $ lines f
    print $ task1 line
    print $ task2 line

