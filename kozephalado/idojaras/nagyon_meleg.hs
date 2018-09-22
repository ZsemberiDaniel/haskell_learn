townCount = 3
dayCount = 5

days = [
    [10, 15, 12, 10, 10],
    [11, 11, 11, 11, 20],
    [18, 16, 16, 16, 20]
       ]

whichIsWarmer coldestTemps coldestOfWarm = filter (>(coldestOfWarm, maxBound :: Int)) coldestTemps

-- the coldest day among the warmest ones
coldestOfWarmests townData = minimum (warmest townData)

tupleDays l = [(l !! i, i) | i <- [0..(townCount - 1)]]

coldest townData = [minimum tD | tD <- townData]
warmest townData = [maximum tD | tD <- townData]

solve = ((map (succ . snd) (whichIsWarmer (tupleDays $ coldest days) (coldestOfWarmests days))) ++ [-1]) !! 0