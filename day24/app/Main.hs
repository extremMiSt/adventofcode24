module Main where
import Data.Map (Map, fromList, member, (!), union, size)
import qualified Data.Map as M
import Data.List (isPrefixOf, uncons, (\\), sort)
import Numeric (readBin)
import Debug.Trace (trace)

type Gate = (String,String,String,String)

trace' :: Show a => a -> a
trace' a= trace (show a) a

parse :: [String] -> (Map String Bool, [Gate])
parse f = (fromList $ map parseInit ini, map parseCircuit circuit)
    where
        ini = takeWhile (/= "") f
        circuit = drop 1 $ dropWhile (/= "") f
        parseInit s = (takeWhile (/=':') s, parseB $ drop 1 (dropWhile (/=' ') s))
        parseCircuit s = (head $ words s, words s !! 2, words s !! 1, words s !! 4)

parseB :: String -> Bool
parseB "0" = False
parseB "1" = True
parseB _ = error "this should not happen"

showB :: Bool -> Char
showB True = '1'
showB False = '0'

op :: String -> Bool -> Bool -> Bool
op "OR" = (||)
op "AND" = (&&)
op "XOR" = (/=)
op _ = error "this should not be happening"

step :: Map String Bool -> [Gate] -> Map String Bool
step ass circuit = fromList $ map apply $ filter (\(c1,c2,_,_) -> c1 `member` ass && c2 `member` ass) circuit
    where
        apply (c1,c2,o,r) = (r, op o (ass!c1) (ass!c2))

loop :: Map String Bool -> [Gate] -> Map String Bool
loop ass circuit | ass /= next = loop next circuit
                 | otherwise = next
    where
        next = ass `union` step ass circuit

map2Bin :: (Num k, Ord k) => Map k Bool -> [Char]
map2Bin m = dewit (fromIntegral $ size m - 1)
    where
        dewit n | n == 0 =  [showB (m!n)]
                | otherwise = showB (m!n) : dewit (n-1)

fromMap :: Map Integer Bool -> Integer
fromMap set = fst $ head $ readBin $ map2Bin set

part1 :: Map String Bool -> [Gate] -> Integer
part1 ini circuit = fromMap $ M.mapKeys (read . drop 1) zs
    where
        zs =  M.filterWithKey (\k _ -> "z" `isPrefixOf` k) $ loop ini circuit

out :: (a, b, c, d) -> d
out (_,_,_,v) = v

isOutput :: Gate -> String -> Bool
isOutput (_,_,_,r) str = r ==str

isInput :: Gate -> String -> Bool
isInput (c1,c2,_,_) str = c1==str || c2==str

maybeHead :: [a] -> Maybe a
maybeHead x = case uncons x of
                Nothing -> Nothing
                Just (v,_) -> Just v

anyMissing :: (Maybe Gate,Maybe Gate,Maybe Gate,Maybe Gate,Maybe Gate) -> Bool
anyMissing (l1andG, l1xorG, l2xorG,l2andG, l3orG) | Just _ <- l1andG,  Just _ <- l1xorG,  Just _ <- l2xorG,  Just _ <- l2andG,  Just _ <- l3orG = False
                                                  | otherwise = True

match :: [Gate] -> Integer -> (Maybe Gate,Maybe Gate,Maybe Gate,Maybe Gate,Maybe Gate)
match circuit n  = (l1andG, l1xorG, l2xorG, l2andG, l3orG)
    where
        nstr = show n
        padded = replicate (2-length nstr) '0' ++ nstr
        xStr = 'x' : padded
        yStr = 'y' : padded
        zStr = 'z' : padded
        l1and = filter (\g@(_,_,o,_) -> isInput g xStr && isInput g yStr && o == "AND") circuit
        l1andG = maybeHead l1and
        l1xor = filter (\g@(_,_,o,_) -> isInput g xStr && isInput g yStr && o == "XOR") circuit
        l1xorG = maybeHead l1xor
        l2xor | Nothing <- l1xorG = []
              | Just v <- l1xorG = filter (\g@(_,_,o,_) -> isInput g (out v) &&  isOutput g zStr && o == "XOR") circuit
        l2xorG = maybeHead l2xor
        l2and | Nothing <- l1xorG = []
              | Just v <- l1xorG = filter (\g@(_,_,o,_) -> isInput g (out v) && o == "AND") circuit
        l2andG = maybeHead l2and
        l3or | Just v1 <- l1andG, Just v2 <- l2andG = filter (\g@(_,_,o,_) -> isInput g (out v1) && isInput g (out v2) && o == "OR") circuit
             | otherwise = []
        l3orG = maybeHead l3or

tuple2List :: (a, a, a, a, a) -> [a]
tuple2List (l1andG, l1xorG, l2xorG,l2andG, l3orG) = [l1andG, l1xorG, l2xorG,l2andG, l3orG]

fromMaybeList :: [Maybe a] -> [a]
fromMaybeList (x:xs) | Just y <- x = y : fromMaybeList xs
                     | Nothing <- x = fromMaybeList xs
fromMaybeList [] = []

collectCorrect :: [(Maybe Gate, Maybe Gate, Maybe Gate, Maybe Gate, Maybe Gate)] -> [Gate]
collectCorrect = concatMap (fromMaybeList .tuple2List)

incorrect :: [Gate] -> [(Maybe Gate, Maybe Gate, Maybe Gate, Maybe Gate, Maybe Gate)] -> [Gate]
incorrect circuit matches = circuit \\ collectCorrect matches

display :: [[Char]] -> [Char]
display (x:y:xs) = x++","++ display (y:xs)
display [x] = x
display [] = []

main :: IO ()
main = do
    (ini, circuit) <- parse . lines <$> readFile "./input01.txt"
    print $  part1 ini circuit
    let candidates = map (match circuit) [0..44]
    mapM_ (print . show) $ filter anyMissing candidates
    mapM_ (print . show) $ incorrect circuit candidates

    -- it follows that tjp is switched with nrn, z23 with bks, z16 with tdv, and z09 with hnd
    -- see the xournal file
    print $ display $ sort ["tjp", "nrn", "z23", "bks", "z16", "tdv", "z09", "hnd"]
