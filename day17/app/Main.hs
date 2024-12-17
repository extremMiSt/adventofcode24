module Main where
import Data.List.Split (splitOn)
import Data.Bits (xor)
import Debug.Trace (trace)
import Numeric (readBin, showBin)

data State = Running Integer Integer Integer Int (Maybe Integer) | Stopped Integer Integer Integer Int
    deriving (Show,Eq,Ord)

parse :: [[Char]] -> (Integer, Integer, Integer, [Integer])
parse line = (a,b,c, code)
    where
        a = read $ drop 2 $ dropWhile (/= ':') $ head line
        b = read $ drop 2 $ dropWhile (/= ':') $ line!!1
        c = read $ drop 2 $ dropWhile (/= ':') $ line!!2
        code = map read $ splitOn ","  (drop 2 $ dropWhile (/= ':') $ line !!4)

step :: [Integer] -> Integer -> Integer -> Integer -> Int -> State
step code a b c p
    | p >= length code = Stopped a b c p
    | opcode == 0 = Running (a `div` (2^comboval)) b c (p+2) Nothing
    | opcode == 1 = Running a (b `xor` operand) c (p+2) Nothing
    | opcode == 2 = Running a (comboval`mod`8) c (p+2) Nothing
    | opcode == 3 && a == 0 = Running a b c (p+2) Nothing
    | opcode == 3 = Running a b c (fromIntegral operand) Nothing
    | opcode == 4 = Running a (b `xor` c) c (p+2) Nothing
    | opcode == 5 = Running a b c (p+2) (Just (comboval`mod`8))
    | opcode == 6 = Running a (a `div` (2^comboval)) c (p+2) Nothing
    | opcode == 7 = Running a b (a `div` (2^comboval)) (p+2) Nothing
    | otherwise = error "this should not happen"
        where
            opcode = code!!p
            operand = code!!(p+1)
            comboval | operand >=0 && operand <= 3 = operand
                     | operand == 4 = a
                     | operand == 5 = b
                     | operand == 6 = c
                     | otherwise = error "this should not happen"

loop :: [Integer] -> Integer -> Integer -> Integer -> Int -> [Integer]
loop code a b c p | Stopped {}  <- next = []
                  | Running a' b' c' p' Nothing <- next = loop code a' b' c' p'
                  | Running a' b' c' p' (Just o) <- next = o : loop code a' b' c' p'
    where
        next = step code a b c p

fromBinString :: (Eq a, Num a) => String -> a
fromBinString str = fst $ head $ readBin str

tbs :: Integral a => a -> String
tbs i = off++bin
    where 
        bin = showBin i ""
        off = take (3 - length bin) "000"

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (a,b,c,code) = parse $ lines f
    print $ loop code a b c 0
    print $ code
    print $ loop code 
    return ()
