import Data.List (foldl', elemIndex, minimumBy)
import Data.Maybe (isNothing, fromJust)
import Data.Function (on)

type Node  = Int
type Edge  = (Node, Node)
type Graph = [Edge]
type Path  = [Node]

graph1 :: Graph
graph1 = [(1,2),(1,3),(3,4),(3,5),(4,6),(5,6),(2,3),(2,4)]

graph2 :: Graph
graph2 = [(2,3),(3,4),(5,1),(1,6),(7,8),(8,9),(2,5),(5,7),(3,1),(1,8),(4,6),(6,9)]

graph3 :: Graph
graph3 = [(7,2),(2,3),(3,4),(5,1),(1,6),(1,4),(7,8),(8,9),(2,5),(5,7),(3,1),(1,8),(4,6),(6,9),(9,8)]

uInsert :: Ord a => a -> [a] -> [a]
uInsert e [] = [e]
uInsert e l@(x:xs)
    | e == x = l
    | e < x = e:l
    | otherwise = x:(uInsert e xs)

nodes :: Graph -> [Node]
nodes = foldl' (\acc (a, b) -> (uInsert b) $ uInsert a acc) []

areNeighbouringNodes :: Graph -> Node -> Node -> Bool
areNeighbouringNodes g n1 n2 = any (\edge -> edge == (n1, n2) || edge == (n2, n1)) g

neighbouringNodes :: Graph -> Node -> [Node]
neighbouringNodes g n = filter (areNeighbouringNodes g n) (nodes g)

continuations :: Graph -> Path -> [Node]
continuations g (ln:l) = filter (`notElem` l) (neighbouringNodes g ln)

fork :: Graph -> Path -> [Path]
fork g path = map (:path) (continuations g path)

initPaths :: Graph -> Node -> [Path]
initPaths g n
    | n `elem` nodes g = [[n]]
    | otherwise = error "Invalid node!"

forkPaths :: Graph -> [Path] -> [Path]
forkPaths g paths = concat $ map (fork g) paths

generatePaths :: Graph -> Node -> [Path]
generatePaths g n = recur (initPaths g n)
    where recur paths = concat $ map (\p -> if fork g p == [] then [p] else recur $ fork g p) paths

getDirections :: Graph -> Node -> Node -> [Path]
getDirections g n1 n2 = map (\(i, l) -> drop (fromJust i) l) $ filter (not . isNothing . fst) n2Indices
    where allPaths = generatePaths g n1
          n2Indices = map (\p -> ((elemIndex n2 p), p)) allPaths

shortestPath :: Graph -> Node -> Node -> Path
shortestPath g n1 n2 = (minimumBy (compare `on` length)) $ getDirections g n1 n2
