module Main where
import Data.Bits (xor)
import Data.Map (empty, Map, member, insert, unionsWith, toList)

derive :: Integer -> Integer
derive n = next
    where
        m = (n `xor` (n * 64)) `mod` 16777216
        d = (m `xor` (m `div` 32)) `mod` 16777216
        next = (d `xor` (d*2048)) `mod` 16777216

rep :: Int -> (a->a) -> (a->a)
rep n f | n == 0 = id
        | otherwise = f . rep (n-1) f

derive2000 :: Integer -> Integer
derive2000 = rep 2000 derive

part1 :: [Integer] -> Integer
part1 f = sum $ map derive2000 f

diffs :: Integer -> [(Integer,Integer)]
diffs ini = zip (zipWith (-) (tail d) d) (tail d)
    where
        d = map ((\x -> read [x]) . last . show) . take 2001 $ ite
        ite = iterate derive ini

insertNew :: Ord k => k -> v -> Map k v -> Map k v
insertNew k v m | k `member` m = m
                | otherwise  = insert k v m

quadruplets ::  Map (Integer,Integer,Integer,Integer) Integer -> [(Integer,Integer)] -> Map (Integer,Integer,Integer,Integer) Integer
quadruplets m ((x1,_):p2@(x2,_):p3@(x3,_):p4@(x4,price):xs) = quadruplets (insertNew (x1,x2,x3,x4) price m) (p2:p3:p4:xs)
quadruplets m _ = m

part2 :: [Integer] -> Integer
part2 f = maximum $ map snd $ toList $ unionsWith (+) $ map (quadruplets empty. diffs) f

main :: IO ()
main = do
    f <- map read . lines <$> readFile "./input01.txt"
    print $ part1 f
    print $ part2 f
