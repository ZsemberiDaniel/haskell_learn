import Data.Char (toUpper)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

morseTab :: [(Char, String)]
morseTab =
  [('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',".")
  ,('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---")
  ,('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---")
  ,('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-")
  ,('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-")
  ,('Y',"-.--"),('Z',"--..")
  ]

normalizeText :: String -> String
normalizeText text = foldl addText [] text
    where addText acc char = acc ++ (if isLetter char then [Data.Char.toUpper char] else [])
          isLetter char = ('a' <= char && char <= 'z') || ('A' <= char && char <= 'Z')

charToCode :: [(Char,String)] -> Char -> String
charToCode mTab char = snd $ fromJust $ find ((== char) . fst) mTab

encodeToWords :: String -> [String]
encodeToWords = map (charToCode morseTab)

encodeString :: String -> String
encodeString = concat . encodeToWords

codeToChar :: [(a,String)] -> String -> a
codeToChar mTab code = fst $ fromJust $ find ((== code) . snd) mTab

decodeWords :: [String] -> String
decodeWords = map (codeToChar morseTab)

-- point free
-- const
-- ap f g x = f x (g x)

withShortestCodes :: [(Char,String)] -> [Char]
withShortestCodes morseTab = map fst (filter ((== smallest) . snd) lengthMapped)
    where lengthMapped = map (\elem -> (fst elem, length $ snd elem)) morseTab
          smallest = minimum $ map snd lengthMapped

getPossiblePrefixes :: [(Char,String)] -> String -> [(Char,String)]
getPossiblePrefixes morseTab code = filter ((`isPrefixOf` code) . snd) morseTab

decodeString :: String -> [String]
decodeString [] = [[]]
decodeString code = concat $ map (\(cLetter, cCode) -> concatToAll cLetter $ decodeString $ leftOverCode cCode code) possiblePrefixes
    where possiblePrefixes = getPossiblePrefixes morseTab code
          leftOverCode currCode = drop (length currCode)
          concatToAll what = map (what:)
