import Data.List
import Data.Maybe

nextRational :: Rational -> Rational
nextRational numb = 1 / (2 * (fromIntegral $ floor numb) + 1 - numb)

positiveRationals :: [Rational]
positiveRationals = 1:(map nextRational positiveRationals)

rationals :: [Rational]
rationals = 0:(concat [[nextRat !! i, -1 * nextRat !! i] | i <- [0..]])
    where nextRat = positiveRationals

rationalIndex :: Rational -> Int
rationalIndex nmb = Data.Maybe.fromJust $ Data.List.findIndex (==nmb) rationals

index :: Eq a => a -> [a] -> Int
index elem list = fst $ filter ((elem ==) . snd) (zip [0..] list) !! 0