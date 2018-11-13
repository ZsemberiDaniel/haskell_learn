map' :: (a -> b) -> [a] -> [b]
map' f (x:l) = (f x):(map' f l)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:l)
    | f x = x:(filter' f l)
    | otherwise = filter' f l

count' :: (a -> Bool) -> [a] -> Int
count' f = length . (filter' f)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:l)
    | f x = x:(takeWhile' f l)
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f a@(x:l)
    | f x = dropWhile' f l
    | otherwise = a

span' :: (a -> Bool) -> [a]{-véges-} -> ([a],[a])
span' f [] = ([], [])
span' f (x:l)
    | f x = (x:a, b)
    | otherwise = ([], l)
        where (a, b) = span' f l

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:(iterate' f $ f x)

infixr 0 $$
($$) :: (a -> b) -> a -> b
($$) f x = f x

all' :: (a -> Bool) -> [a]{-véges-} -> Bool
all' f = and . (map f) 

any' :: (a -> Bool) -> [a]{-véges-} -> Bool
any' f = not . null . (filter f)

elem' :: Eq a => a -> [a]{-véges-} -> Bool
elem' e = any (==e)

filters' :: Eq a => [a] -> [a] -> [a]
filters' f = filter' (`elem` f)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f [] b = []
zipWith' f a [] = []
zipWith' f (x:a) (y:b) = (f x y):(zipWith' f a b)

differences' :: Num a => [a] -> [a]
differences' a@(x:l) = zipWith' (-) l a

fibPairs :: [(Integer, Integer)]
fibPairs = iterate' (\(a, b) -> (b, a + b)) (0, 1)

group :: Eq a => [a]{-véges-} -> [[a]]
group [] = []
group [x] = [[x]]
group (x:l)
    | head a == x = (x:a):r
    | otherwise = [x]:all
        where all@(a:r) = group l

compress :: Eq a => [a] -> [(Int,a)]
compress = map (\x -> (length x, head x)) . group

pascalTriangle :: [[Integer]]
pascalTriangle = [1]:(iterate' (\l -> 1:(add l) ++ [1]) [1,1])
    where add a@(x:l) = zipWith' (+) l a

sqrt' :: RealFloat a => a -> a
sqrt' a = last $ take 100 $ iterate' (\x -> (x + a / x) / 2) a

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b

decompress' :: Eq a => [(Int,a)] -> [a]
decompress' [] = []
decompress' ((c, x):l) = (replicate c x) ++ (decompress' l)

weightedAvg :: Fractional a => [(a,a)] -> a
weightedAvg l = s / c
    where (c, s) = foldl (\(c, s) (cx, sx) -> (c + cx, s + c * sx)) (0, 0) l
