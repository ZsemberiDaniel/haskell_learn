import Debug.Trace (trace)
import Data.List (group)

problem1 :: [a] -> a
problem1 l = l !! (length l - 1)

problem2 :: [a] -> [a]
problem2 l = take (length l - 1) l

-- elem at index in list
problem3 :: [a] -> Int -> a
problem3 list at = list !! at

-- length of list
problem4 :: [a] -> Int
problem4 [] = 0
problem4 (elem:list) = 1 + problem4 list

-- reverse list
problem5 :: [a] -> [a]
problem5 [] = []
problem5 (elem:list) = problem5 list ++ [elem]

-- palindrome
problem6 :: (Eq a) => [a] -> Bool
problem6 list = list == reverse list

-- remove duplicates after each other
problem9 :: (Eq a) => [a] -> [a]
problem9 (x:y:list)
    | x == y = (problem9 $ y:list)
    | otherwise = x:(problem9 $ y:list)
problem9 (x:[]) = [x]

-- Run-length encoding
problem10 :: [Char] -> [(Char, Int)]
problem10 list = [(head x, length x) | x <- group list]