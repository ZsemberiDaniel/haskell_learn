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