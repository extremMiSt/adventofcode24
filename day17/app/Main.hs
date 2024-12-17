module Main where
import Data.List.Split (splitOn)
import Data.Bits (xor)
import Numeric (readBin, showBin)
import Data.List (isSuffixOf)
import Debug.Trace (trace)

trace' :: Show a => a -> a
trace' a = trace (show a) a

traceS :: Show a => String -> a -> a
traceS str a = trace (str ++ show a) a

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

part1 :: [Integer] -> Integer -> Integer -> Integer -> [Integer]
part1 code a b c= loop code a b c 0

fromBinString :: String -> Integer
fromBinString "" = 0
fromBinString str = fst $ head $ readBin str

tbs :: Integer -> String
tbs i = off++bin
    where 
        bin = showBin i ""
        off = take (3 - length bin) "000"

candidates :: [Integer] -> [Char] -> [Integer]
candidates code done  = map fst $ filter (\(_,r) -> r `isSuffixOf` code) c
    where
        c = map (\x -> (x,loop code (fromBinString $ done ++ tbs x) 0 0 0)) [0..7]

makeInput :: [Integer] -> String -> [Integer] -> String
makeInput _ _ [] = error "this should not happen"
makeInput code done (x:xs) | null candidates' && code == loop code (fromBinString extended) 0 0 0 = extended
                           | null candidates' = makeInput code done xs
                           | otherwise = makeInput code (done ++ tbs x) candidates'
    where 
        extended = done ++ tbs x
        candidates' = candidates code extended

part2 :: [Integer] -> Integer
part2 code = fromBinString $ makeInput code "" (candidates code "")

main :: IO ()
main = do
    f <- readFile "./input01.txt"
    let (a,b,c,code) = parse $ lines f
    print $ part1 code a b c
    print $ part2 code
