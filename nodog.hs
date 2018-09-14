bmi :: (Fractional a) => a -> a -> a
bmi weight height = weight / height ^ 2

insultMe :: (Ord a, Fractional a) => a -> [Char]
insultMe bmi
    | bmi <= 18.5 = "Not even good at eating"
    | bmi <= 25.0 = "As boring as this lesson"
    | bmi <= 30 = "Fatty fat fat fat... fat."
    | otherwise = ""

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (num:list) = num + sum' list

length' :: (Num a) => [a] -> a
length' [] = 0
length' (num:list) = 1 + length' list

whatsThisList :: (Show a) => [a] -> [Char]
whatsThisList list
    | length list == 0 = "This list is empty my boy"
    | length list == 1 = "The list contains exactly one element and that is " ++ (concat (replicate 100 ".")) ++ show (list !! 0)
    | otherwise = "I don't know man, this list is pretty generic"