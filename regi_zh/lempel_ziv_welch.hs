import Data.Char (chr)
import Data.List (isPrefixOf)

dictionary :: [String]
dictionary = map ((:[]) . Data.Char.chr) [0..127]

prefixes :: String -> [String] -> [(Int,String)]
prefixes word dict = filter (flip Data.List.isPrefixOf word . snd) (zip [0..] dict)

longest :: [(Int,String)] -> (Int,String)
longest prefs = foldr maxLength (last prefs) prefs
    where maxLength a b = if snd a > snd b then a else b

munch :: [String] -> String -> (Int,String,String)
munch dict word = (longestInd, longestVal, drop (length longestVal) word)
    where l = longest (prefixes word dict)
          longestInd = fst l
          longestVal = snd l

append :: [String] -> String -> String -> [String]
append dict pref wordRest = dict ++ (if newWord `elem` dict then [] else [newWord])
    where newWord = pref ++ (if null wordRest then [] else [head wordRest])

encode :: [String] -> String -> [Int]
encode dict word
    | length dict >= 256 = []
    | null word = []
    | otherwise = munchedInd:(encode newDict munchedVal)
        where (munchedInd, munchedPref, munchedVal) = munch dict word
              newDict = append dict munchedPref munchedVal

compress :: String -> String
compress word = foldl (\acc elem -> acc ++ [Data.Char.chr elem]) [] encoded
    where encoded = encode dictionary word

decode :: [String] -> [Int] -> String
decode dict [] = []
decode dict (elem:[]) = [dict !! elem]
decode dict (A:B:list)