import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Function (on) h7

main = do
    workCount <- getLine
    -- get all works
    works <- replicateM (read workCount) inputLineNumbers

    -- convert works to tuples (id, tillDay, pay)
    let workTuples = [(i, works !! i !! 0, works !! i !! 1) | i <- [0..read workCount - 1]]
        sortedWorkTuples = workSort workTuples

    putStrLn $ show sortedWorkTuples

occupyDay calendar 1 = calendar
occupyDay calendar day 
    | calendar !! day = occupyDay calendar $ day - 1
    | otherwise = 

-- sorts the list of work tuples in the exercise
workSort :: [(Int, Int, Int)] -> [(Int, Int, Int)]
workSort = sortBy (flip compare `on` workPay)

-- id of work tuple
workId :: (Int, Int, Int) -> Int
workId (id, _, _) = id

-- till when the work can be completed from the tuple
workTillDay :: (Int, Int, Int) -> Int
workTillDay (_,till,_) = till

-- the pay of the worktuple
workPay :: (Int, Int, Int) -> Int
workPay (_,_,pay) = pay

-- Gets an input line from console and converts them to a number list
-- example: "23 45 34" -> [23, 45, 34]
inputLineNumbers = do
    line <- getLine
    return [read word :: Int | word <- words line]