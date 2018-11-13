data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq, Enum)

isWorkday :: Day -> Bool
isWorkday Sat = False
isWorkday Sun = False
isWorkday _ = True
-- isWorkday d = d `notElem` [Sat, Sun]  -- Eq required
-- isWorkday d = d `elem` [Mon .. Fri] -- Enum required

-- Enum
--  fromEnum Mon == 0
--  succ Tue == Wed
--  toEnum 0 :: Day == Mon

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False
-- isWeekend = not . isWorkday

type Hour = Int
type Minute = Int
data Time = T Hour Minute
    deriving (Eq)

instance Show Time
    where show = showTime

showTime :: Time -> String
showTime (T h m) = helper h ++ ":" ++ helper m
    where helper x
            | x < 10 = '0':(show x)
            | otherwise = show x

-- map (show . ( `T` 0)) [0..23]
-- map (show . flip T 0) [0..23]

-- not needed because built-in Eq does the same
-- instance Eq Time
--      where (==) = eqTime
eqTime :: Time -> Time -> Bool
eqTime (T h1 m1) (T h2 m2) = h1 == h2 && m1 == m2

isEarlier :: Time -> Time -> Bool
isEarlier (T h1 m1) (T h2 m2) = (h1 < h2) || (h1 == h2 && m1 < m2)

isBetween :: Time -> Time -> Time -> Bool
isBetween t1 t2 t3 = isEarlier t2 t1 && isEarlier t1 t3

time :: Hour -> Minute -> Time
time h m
    | h < 0 || h >= 24 || m < 0 || m >= 60 = error "Incorrect time parameters!"
    | otherwise = T h m

data USTime = AM Hour Minute | PM Hour Minute

instance Show USTime
    where show (AM h m) = show (T h m) ++ " AM"
          show (PM h m) = show (T h m) ++ " PM"

toUsTime :: Time -> USTime
toUsTime (T h m)
    | h > 12 = PM (h - 12) m
    | h == 0 = AM 12 m
    | otherwise = AM h m

toTime :: USTime -> Time
toTime (AM h m) = T h m
toTime (PM h m) = T (h + 12) m
