(%^) :: Bool -> Bool -> Bool
x %^ y = (x||y) && (not(x&&y))


part :: Int -> [[Int]]
part 0 = [[]]
part n = [(x:xs) | x <- [1..n], xs <- part (n - x)]

-- ____________________________________ 1 ___________________________________________
-- triangle with right angle and perimeter of 24 (all sides under 10)
triangle = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a * a + b * b == c * c, a + b + c == 24]

-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK k l = k l

-- Determine if list l is a palindrome
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate xs = concat [replicate 2 a | a <- xs]

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike [] ys = []
ziplike xs [] = []
ziplike (x:xs) (y:ys) = (x, y):(ziplike xs ys)
    
    --[(xs !! i, ys !! i) | i <- [0..min (length xs) (length ys) - 1]]

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK k l = take k l ++ drop (k + 1) l

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice i k l = take (k - i) (drop i l)

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem x k l = take k l ++ (x:(drop k l))

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate n l = drop (length l - n) l ++ take (length l - n) l

-- ____________________________________ 2 ___________________________________________
{-
 - For this exercise, we are dealing with a type for colours of the rainbow
 - The typeclass is defined here, and note its English spelling.
 - For more information on how this is done, look ahead to:
 - http://learnyouahaskell.com/making-our-own-types-and-typeclasses
 -
 - Have a play with the Colour in ghci, try the succ and pred functions and so on.
 -}
data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
            deriving (Eq, Ord, Show, Bounded, Enum)   

{-
- Again, you should be able to write these functions in one line, 
- using the information from the chapter http://learnyouahaskell.com/types-and-typeclasses
- and the chapter before
-}

{-
- The Colour typeclass is of type Ord
- What is the "first" (or least) colour
-}
firstColor :: Color
firstColor = minBound :: Color
lastColor :: Color
lastColor = maxBound :: Color

-- List the colours in reverse order
reverseColorOrder :: [Color]
reverseColorOrder = reverse [firstColor..lastColor]

{-
- Mix two colours together, to produce the average value of the two.
- Example: paintMix Orange Green = Yellow
- If necessary, favour the "higher" value when computing the average.
- For example: paintMix Green Violet = Indigo
- Hint: Integer division can be performed with the quot function: quot 7 2 = 3
-}
middleElement :: [a] -> a
middleElement a = a !! (length a `div` 2)

paintMix :: Color -> Color -> Color
paintMix c1 c2 = middleElement (if c1 < c2 then [c1..c2] else [c2..c1])

-- ____________________________________ 4 ___________________________________________
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

-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit 0 = "zero"
englishDigit 1 = "one"
englishDigit 2 = "two"
englishDigit 3 = "three"
englishDigit 4 = "four"
englishDigit 5 = "five"
englishDigit 6 = "six"
englishDigit 7 = "seven"
englishDigit 8 = "eight"
englishDigit 9 = "nine"
englishDigit x = "unknown"
    
-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y) 
    | y == 0 = undefined
    | otherwise = x / y
    
-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (x:y:z:xs)
    | x == y && y == z && z == 0 = True
    | otherwise = False
threeZeroList xs = False



-- ____________________________________ 5 ___________________________________________
-- Raise x to the power y, using recursion
-- For example, power 5 2 = 25
power :: Int -> Int -> Int
power x 1 = x
power x y = x * power x (y - 1)

-- create a list of length n of the fibbonaci sequence in reverse order
-- examples: fib 0 = [0]
-- 	     fib 1 = [1, 0]
--	     fib 10 = [55,34,21,13,8,5,3,2,1,1,0]	
-- try to use a where clause
fib :: (Num a, Eq a) => a -> [a]
fib 0 = [0]
fib 1 = [1, 0]
fib x =
    let seq = fib $ x - 1
    in  (seq !! 0 + seq !! 1):seq

-- This is not recursive, but have a go anyway.
-- Create a function which takes two parameters, a number and a step
-- The result is the sign of the original number reversed, and the step added to the absolute value
-- Confused? Some examples: stepReverseSign 6 2 = -8
--			    stepReverseSign -3 1 = 4
--			    stepReverseSign 1 2 = -3
stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign a b = -sign a * (abs a + b)

sign :: (Fractional a, Ord a) => a -> a
sign a = a / a

{- Lets calculate pi.
 - The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
 - Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
 - We can create a function, where given a certain tolerance, we can recursively calculate
 - Pi to within that tolerance.
 - Lets create two functions, piCalc, and piCalc', the latter we will recursively call
 - until our pi calculation is within the tolerance
 - The piCalc function is defined as:
 - piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
 - Given a tolerance, say, 0.001, it will return a tuple.
 - fst is pi to an accuracy of the tolerance, 0.001 in this case
 - snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
 - Example: piCalc 0.001 = (3.1420924036835256,2000)
 - The piCalc' function is defined as 
 - piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
 - Lots of parameters!
 - The first parameter is the current denominator from the Leibniz formula
 - The next is our calculation of pi from our previous attempt
 - The next is the tolerance
 - The final parameter is the number of times this function has been called (ie, we add one every time we recurse
 - Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)
 -
 - Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
 - But, of course the output of piCalc should remain as (pi, count)
 - 
 - You may find the stepReverseSign function handy
 -}

piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
piCalc a = piCalc' 1 0 a 1

piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
piCalc' denom pie tolerance counter
    | abs (realPi - pie) <= tolerance = (pie, counter)
    | otherwise = piCalc' (denom + 2) (4 / denom * (if counter `mod` 2 == 1 then 1 else -1) + pie) tolerance (counter + 1)

realPi :: (Fractional a) => a
realPi = 3.14159265358979323846264338327950288419716939937510 

-- ___________________________________________ 6 ___________________________________
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n:collatz next
    where next = if odd n then n * 3 + 1 else n `div` 2

collatzLengthCounter till minLength = length (filter isLong (map collatz [1..till]))
    where isLong list = length list >= minLength


-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
sumInts :: Int -> Int -> Int
sumInts a b = foldl (+) 0 [a..b]

-- Define a square function
sq :: Int -> Int
sq x = undefined

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
sumSquares a b = undefined

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b = undefined

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = undefined

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = undefined

-- Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the prodcut of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int
higherOrderSequenceApplication = undefined

-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial = undefined