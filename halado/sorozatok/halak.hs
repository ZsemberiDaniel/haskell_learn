import Control.Monad (replicateM)
import Data.List

main = do
    fishCount <- getLine
    -- get all the lines which describe what time it is forbidden
    nums <- replicateM (read fishCount) inputLineNumbers
    
    -- convert forbidden times to tuples
    let tuples = [(nmbs !! 0, nmbs !! 1) | nmbs <- nums]
    -- solve problem
    let solutions = solve $ sort tuples

    putStrLn $ show solutions

-- Gets an input line from console and converts them to a number list
-- example: "23 45 34" -> [23, 45, 34]
inputLineNumbers = do
    line <- getLine
    return [read word :: Int | word <- words line]

-- solves the problem by combining the times and then reversing them
solve ((tStart, tEnd):times) = reverseTimesFromStart $ combineTimes [(tStart, tEnd)] times

-- reverses times of intervals from the start of the year
reverseTimesFromStart = reverseTimes 1

-- reverses the time intervals in a year for example
-- [(2, 100),(150, 160),(300,365)] -> [(1,1),(101,149),(161,299)]
reverseTimes :: Int -> [(Int, Int)] -> [(Int, Int)]
reverseTimes 365 [] = [] -- finished calculation and 365 is forbidden
reverseTimes timeAt [] = [(timeAt, 365)] -- finished calculation and 365 is not forbidden
reverseTimes timeAt ((tStart, tEnd):times)
    | tStart == 1 = reverseTimes (tEnd + 1) times -- day 1 is forbidden -> (skip first)
    | otherwise = (timeAt, tStart - 1):(reverseTimes (tEnd + 1) times)

-- this function combines the forbidden times (removes overlaps)
-- for example [(1, 3), (2, 4), (5, 6), (10, 13), (10, 14)] -> [(1, 6), (10, 14)]
combineTimes :: (Ord a, Num a) => [(a,a)] -> [(a,a)] -> [(a,a)]
combineTimes out [] = reverse out
combineTimes allOut@((oStart, oEnd):out) ((tStart, tEnd):times)
    | (oStart <= tStart && tStart <= oEnd + 1) =    combineTimes ((oStart, max oEnd tEnd):out) times
    | otherwise =                                   combineTimes ((tStart, tEnd):allOut) times