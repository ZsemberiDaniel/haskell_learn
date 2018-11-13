import Data.List (sort, group, inits, tails)

--(f . g) x = f $ g x

numbersMadeOfOnes :: [Integer]
numbersMadeOfOnes = iterate ((+ 1) . (* 10)) 1

numbersMadeOfThrees :: [Integer]
numbersMadeOfThrees = iterate ((+ 3) . (* 10)) 3

dropSpaces :: String -> String
dropSpaces = dropWhile (== ' ')

trim :: String{-véges-} -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

maximumOfMinimums :: Ord a => [[a]] -> a
maximumOfMinimums = maximum . map minimum

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap = map . map

monogram :: String -> String
monogram = unwords . map ((:".") . head) . words

uniq :: Ord a => [a]{-véges-} -> [a]
uniq = map head . group . sort

repeated :: Ord a => [a]{-véges-} -> [a]
repeated = map head . filter ((>= 2). length) . group . sort

sublists :: [a] -> [[a]]
sublists = concat . map (init . tails) . tail . inits

until :: (a -> Bool) -> (a -> a) -> a -> a
until f g = head . filter f . iterate g

myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter f = foldr (\x acc -> if f x then x:(acc) else acc) []
myFilter f = foldr comp [] where
    comp x acc
        | f x       = x:(acc)
        | otherwise = acc


