module Main where
import Debug.Trace (trace)
import Data.Set (Set, empty, insert, delete)
import qualified Data.Set as Set


tracing :: Show a => a -> a
tracing a = trace (show a) a

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

freeSpace :: (Ord b, Num b) => [b] -> Set (b, b)
freeSpace l = dewit l True 0
    where
        dewit (len:ls) True pos = dewit ls False (pos+len)
        dewit (len:ls) False pos = (len,pos) `insert` dewit ls True (pos+len)
        dewit [] _ _ = empty

fileSpace :: (Num t1, Num t2) => [t1] -> [(t1, t1, t2)]
fileSpace l = dewit l True 0 0
    where
        dewit (len:ls) True pos file = (len, pos, file) : dewit ls False (pos+len) (file+1)
        dewit (len:ls) False pos file = dewit ls True (pos+len) file
        dewit [] _ _ _ = []


part1 :: [Integer] -> Integer
part1 l = checksum $ fragment $ zip [0..] $ expand l


movesTo :: Set (Integer, Integer) -> (Integer,Integer,Integer)-> (Integer,Integer)
movesTo free  (len, pos, f) = case minfree of
                                    Nothing -> (len,pos)
                                    (Just e@(_,p)) -> if p<pos then e else (len,pos)
    where
        minfree = Set.lookupGE (len, 0) free

{--moveAll (e@(len, _, f):files) free | diff == 0 = e `insert` (moveAll files (e `delete` free))
    where
        (l, p) = movesTo f free
        diff = l-len
moveAll [] _ = empty

defragment :: [(Integer,Integer)] -> [(Integer,Integer)]
defragment l = dewit (reverse files) free
    where 
        files = fileSpace l
        free = freeSpace l--}


main :: IO ()
main = do
    f <- readFile "./input01.txt"
    --let line = $ map (\x -> read [x]) "2333133121414131402"
    let line = map (\x -> read [x]) $ head $ lines f
    print $ part1 line