module Main where
import Data.Set (Set, empty, insert, member, fromList, toList, notMember, singleton)
import qualified Data.Set as S
import Data.List (isPrefixOf, nub, delete)

type Edge = (String, String)
type Node = String

parse :: Set Edge -> Set Node -> [String] -> (Set Edge, Set Node)
parse edges nodes [] = (edges, nodes)
parse edges nodes (x:xs) = parse edges' nodes' xs
    where
        start = take 2 x
        end = drop 3 x
        edges' = insert (start,end) edges
        nodes' = insert start $ insert end nodes

threeCliques :: Set Edge -> [Node] -> Edge -> [Set Node]
threeCliques _ [] _ = []
threeCliques edges (n:ns) edge@(start, end) | is && historian clique = clique : threeCliques edges ns edge
                                            | otherwise = threeCliques edges ns edge
    where
        is = ((start, n) `member` edges || (n, start) `member` edges) && ((end, n) `member` edges || (n, end) `member` edges)
        clique = fromList [start, end, n]

historian :: Set Node -> Bool
historian = any ("t" `isPrefixOf`)

part1 :: Set Edge -> Set Node -> Int
part1 edges nodes = length $ nub $ delete empty $ concatMap (threeCliques edges nodeList) (toList edges)
    where
        nodeList = toList nodes


extendCliqueBy :: Set Edge -> Node -> Set Node -> Maybe (Set Node)
extendCliqueBy edges node clique | node `notMember` clique &&  can = Just $ node `insert` clique
                               | otherwise = Nothing
    where
        can = all (\n -> (n,node) `member` edges || (node,n) `member` edges) clique

extendClique :: Set Edge -> [Node] -> Set Node -> [Set Node]
extendClique _ [] _ = []
extendClique edges (x:xs) clique | Nothing <- by = extendClique edges xs clique
                                 | Just v <- by = v: extendClique edges xs clique
    where
        by = extendCliqueBy edges x clique

extend :: Set Edge -> [Node] -> [Set Node] -> [Set Node]
extend _ _ [] = []
extend edges nodes (c:cs) = extendClique edges nodes c ++ extend edges nodes cs

part1' :: Set Edge -> Set Node -> Int
part1' edges nodes = length $ nub $ extend edges nodeL $ extend edges nodeL candidates
    where
        nodeL = toList nodes
        candidates = map singleton $ toList $  S.filter ("t" `isPrefixOf`) nodes

part2 :: Set Edge -> Set Node -> String
part2 edges nodes = join $ toList $ dewit candidates
    where 
        candidates = map singleton $ toList  {-$  S.filter ("t" `isPrefixOf`)-} nodes
        nodeL = toList nodes
        dewit cur | length cur == 1 = head cur
                  | otherwise = dewit $ nub $ extend edges nodeL cur
        join [] = []
        join (x:y:xs) = x ++ "," ++  join (y:xs)
        join (x:_) = x


main :: IO ()
main = do
    (edges, nodes) <- parse empty empty . lines <$> readFile "./example01.txt"
    print $ part1 edges nodes
    print $ part2 edges nodes

    return ()

