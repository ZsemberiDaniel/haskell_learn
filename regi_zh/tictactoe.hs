import Data.List (groupBy, sortBy)
import Data.Function (on)

type Cell = (Int, Int)
type Grid = [Cell]
type Player = Char
type State = ([Cell], [Cell], Int)

cells :: Int -> Grid
cells n
    | n <= 0    = []
    | otherwise = [ (i, k) | i <- [0..n -1], k <- [0..n-1] ]

rows :: Int -> [[Cell]]
rows = groupBy ((==) `on` fst) . cells

cols :: Int -> [[Cell]]
cols = groupBy ((==) `on` snd) . sortBy (compare `on` snd) . cells

diags :: Int -> [[Cell]]
diags n 
    | n <= 0    = [[], []]
    | otherwise = [map (\i -> (i, i)) [0..n-1], map (\i -> (i, n - i)) [0..n-1]]

validatePlayer :: (Player -> a) -> Player -> a
validatePlayer f p
    | isValid = f p
    | otherwise = error $ "illegal player " ++ [p]
        where isValid = p == 'o' || p == 'O' || p == 'x' || p == 'X'

otherPlayer :: Player -> Player
otherPlayer 'O' = 'X'
otherPlayer 'o' = 'x'
otherPlayer 'X' = 'o'
otherPlayer 'x' = 'o'
otherPlayer p = error $ "illegal player" ++ [p]

initState :: Int -> State
initState n = ([], [], n)

isValidCell :: Int -> Cell -> Bool
isValidCell n (x, y) = 0 <= x && x < n && 0 <= y && y < n

isCellFree :: State -> Cell -> Bool
isCellFree (os, xs, n) c = c `notElem` os && c `notElem` xs

isValidStep :: State -> Cell -> Bool
isValidStep s@(_, _, n) c = isValidCell n c && isCellFree s c

step :: Player -> State -> Cell -> State
step p s@(os, xs, n) c
    | isValidStep s c = validatePlayer (\_ -> makeStep) p
    | otherwise = error ("invalid move " ++ show c ++ " of player " ++ [p])
        where makeStep
                | p == 'o' || p == 'O' = (c:os, xs, n)
                | p == 'x' || p == 'X' = (os, c:xs, n)

freeCells :: State -> [Cell]
freeCells s@(os, xs, n) = filter (isCellFree s) $ cells n

steps :: Player -> State -> [State]
steps p s@(os, xs, n) = map (step p s) $ freeCells s

won :: Player -> State -> Bool
won p s@(os, xs, n) = (allIn $ rows n) || (allIn $ cols n) || (allIn $ diags n)
    where listToCheck = if p == 'x' || p == 'X' then xs else os
          allIn a = or $ map (and . map (`elem` listToCheck)) a

fork :: (Player, [State], [State]) -> (Player, [State], [State])
fork (p, unfinishedGames, finishedGames) = (otherPlayer p, c, a ++ finishedGames)
    where a = filter (\s -> won 'x' s || won 'o' s) unfinishedGames
          c = concat $ map (steps p) (filter (\s -> not (won 'x' s || won 'o' s)) unfinishedGames)
