data Code = Code Char Char Char Char deriving (Show, Eq)

blackStones :: (Eq a) => [a] -> [a] -> Int
blackStones [] [] = 0
blackStones (l1:l) (l2:k)
    | l1 == l2 = 1 + blackStones l k
    | otherwise = blackStones l k

whiteStones :: (Eq a) => [a] -> [a] -> Int
whiteStones l1 l2 = foldl (\acc n -> if n `elem` rl2 then acc + 1 else acc) 0 rl1
    where filtered = (filter (\(a, b) -> a /= b) (zip l1 l2))
          rl1 = map fst filtered
          rl2 = map snd filtered

readCode :: String -> Maybe Code
readCode w
    | length w == 4 = Just $ Code (w !! 0) (w !! 1) (w !! 2) (w !! 3)
    | otherwise = Nothing

toList :: Code -> [Char]
toList (Code a b c d) = [a, b, c, d]

whiteAndBlackStones :: Code -> String -> (Int,Int)
whiteAndBlackStones code input
    | cCode == Nothing = (0, 0)
    | otherwise = (whiteStones input (toList code), blackStones input (toList code))
        where cCode = readCode input
