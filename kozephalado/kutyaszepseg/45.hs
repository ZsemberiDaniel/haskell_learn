import Control.Monad (replicateM)

main = do
    -- input
    inp <- readLine
    maxPoints <- readLine
    minPoints <- readLine

    -- Make variables for input data
    let dogCount = inp !! 0
        categoryCount = inp !! 1
    
    pointsUnfiltered <- replicateM dogCount readLine

    -- Filter out the dogs that did not reach minimum points in categories
    -- then see whether the points have multiple maximum points
    let isThere = mapWithMax (filterOutMin minPoints pointsUnfiltered)

    putStrLn isThere

mapWithMax :: [[Int]] -> [Char]
mapWithMax points = mapWithMax' points (-1) 0

-- this function first sums up the points array and then returns whether
-- it has multiple max values
mapWithMax' :: [[Int]] -> Int -> Int -> [Char]
mapWithMax' [] _ maxCounter
    | maxCounter > 1 = "VAN"
    | otherwise = "NINCS"
-- this is where the summing and counting happens
mapWithMax' (curr:points) max maxCounter
    | currSum > max = mapWithMax' points currSum 1 -- new max found
    | currSum == max = mapWithMax' points currSum $ maxCounter + 1 -- max found again
    | otherwise = mapWithMax' points max maxCounter -- not a max
        where currSum = sum curr

filterOutMin :: [Int] -> [[Int]] -> [[Int]]
filterOutMin _ [] = []
filterOutMin minPoints (points:allPoints)
    -- see whether all points are bigger than the min
    | foldl isAllAtLeastMin True $ zip [0..] points = points:(filterOutMin minPoints allPoints)
    | otherwise = filterOutMin minPoints allPoints
        where isAllAtLeastMin accul (index, elem)
                | not accul = False
                | otherwise = elem >= minPoints !! index
    
-- reads a line of numbers and seperates them to a list
readLine = do
    line <- getLine
    return [read word :: Int | word <- words line]