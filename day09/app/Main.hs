module Main where
import Debug.Trace (trace)
import Data.List (delete, insert, sort)


tracing :: Show a => a -> a
tracing a = trace (show a) a

tracing' :: Show a => String -> a -> a
tracing' s a = trace (s ++ show a) a

expand :: [Integer] -> [Integer]
expand line = dewit line True 0
    where
        dewit (0:as) True n = dewit as False (n+1)
        dewit (a:as) True n = n : dewit (a-1:as) True n
        dewit (0:as) False n = dewit as True n
        dewit (a:as) False n = -1: dewit (a-1:as) False n
        dewit [] _ _= []


checksum :: (Eq t, Num t) => [t] -> t
checksum l = dewit l 0
    where
        dewit (-1:as) n = dewit as (n+1)
        dewit (a:as) n = a*n + dewit as (n+1)
        dewit [] _ = 0

fragment :: [(Integer,Integer)] -> [Integer]
fragment l = dewit l (reverse l)
    where
        dewit ((i1, f1):l1) ((i2, f2):l2) | i1 > i2 = []
                                          | i1 == i2 = [f1]
                                          | f2 == -1 = dewit ((i1, f1):l1) l2
                                          | f1 /= -1 = f1 : dewit l1 ((i2, f2):l2)
                                          | f1 == -1 = f2 : dewit l1 l2
        dewit _ _ = error "this should not happen"

part1 :: [Integer] -> Integer
part1 l = checksum $ fragment $ zip [0..] $ expand l

freeSpace :: [Integer] -> [(Integer, Integer)]
freeSpace l = dewit l True 0
    where
        dewit (len:ls) True pos = dewit ls False (pos+len)
        dewit (len:ls) False pos = (pos, len) : dewit ls True (pos+len)
        dewit [] _ _ = []

fileSpace :: [Integer] -> [(Integer, Integer, Integer)]
fileSpace l = dewit l True 0 0
    where
        dewit (len:ls) True pos file = (pos, len, file) : dewit ls False (pos+len) (file+1)
        dewit (len:ls) False pos file = dewit ls True (pos+len) file
        dewit [] _ _ _ = []

movesTo :: [(Integer, Integer)] -> (Integer,Integer,Integer)-> (Integer,Integer)
movesTo free (pos, len, _) = case minFree of
                                (x:_) -> x
                                [] -> (pos,len)
    
    where
        minFree = filter (\(p,l) -> len <= l && p<pos) free

moveAll :: [(Integer,Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer,Integer)]
moveAll (file@(_,len,f):files) free | diff == 0 = (p,l,f): moveAll files (delete goal free)
                                    | otherwise = (p,len,f): moveAll files (insert (p+len, diff) (delete goal free))
    where 
        goal@(p,l) = movesTo free file
        diff = l-len
moveAll [] _ = []

defragment :: [Integer] -> [(Integer,Integer,Integer)]
defragment l = sort $ moveAll files free
    where 
        files = reverse $ fileSpace l
        free = freeSpace l

expand' :: [(Integer,Integer,Integer)] -> [Integer]
expand' line = dewit line 0
    where
        dewit ((_,0,_):as) n = dewit as n
        dewit a@((pos,len,file):as) n | n < pos = -1 : dewit a (n+1)
                                      | otherwise = file : dewit ((pos,len-1,file):as) (n+1)
        dewit [] _ = []

part2 :: [Integer] -> Integer
part2 l = checksum $ expand' $ defragment l

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let line = map (\x -> read [x]) $ head $ lines f
    --let line = map (\x -> read [x]) "2333133121414131402"
    print $ part1 line
    print $ part2 line