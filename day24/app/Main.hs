module Main where
import Data.Map (Map, fromList, member, (!), union, size)
import qualified Data.Map as M
import Data.List (isPrefixOf)
import Numeric (readBin)
import Debug.Trace (trace)

trace' :: Show a => a -> a
trace' a= trace (show a) a

parse :: [String] -> (Map String Bool, [(String, String, String, String)])
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

step :: Map String Bool -> [(String, String, String, String)] -> Map String Bool
step ass circuit = fromList $ map apply $ filter (\(c1,c2,_,_) -> c1 `member` ass && c2 `member` ass) circuit
    where
        apply (c1,c2,o,r) = (r, op o (ass!c1) (ass!c2))

loop :: Map String Bool -> [(String, String, String, String)] -> Map String Bool
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

part1 :: Map String Bool -> [(String, String, String, String)] -> Integer
part1 ini circuit = fromMap $ M.mapKeys (read . drop 1) zs
    where
        zs =  M.filterWithKey (\k _ -> "z" `isPrefixOf` k) $ loop ini circuit

main :: IO ()
main = do
    (ini, circuit) <- parse . lines <$> readFile "./input01.txt"
    print $  part1 ini circuit

    print $ (:) ' ' $ map2Bin $ M.mapKeys ((read::String -> Integer) . drop 1) $ M.filterWithKey (\k _ -> "x" `isPrefixOf` k) ini
    print $ (:) ' ' $ map2Bin $ M.mapKeys ((read::String -> Integer) . drop 1) $ M.filterWithKey (\k _ -> "y" `isPrefixOf` k) ini
    print $ map2Bin $ M.mapKeys ((read::String -> Integer) . drop 1) $ M.filterWithKey (\k _ -> "z" `isPrefixOf` k) $ loop ini circuit
    return ()