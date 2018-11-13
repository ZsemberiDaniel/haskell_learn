import Data.List (group, sort, sortBy)
import Data.Ord (compare)
import Data.Function (on)

type CodeTab = [TabEntry]
type TabEntry = (Char, Code)
type Code = [Bit]
data Bit = Zero | One
    deriving (Eq,Show)

getFrequencies :: String -> [(Int, Char)]
getFrequencies = map (\cs -> (length cs, head cs)) . group . sort

orderByFrequency :: String -> [Char]
orderByFrequency = map snd . (sortBy comp) . getFrequencies
    where comp (f1, c1) (f2, c2) = if f1 == f2 then compare c1 c2 else compare f2 f1
    
nextPrefixCode :: Code -> Code
nextPrefixCode code = (replicate (length code) One) ++ [Zero]

prefixCodes :: [Code]
prefixCodes = [Zero]:(map nextPrefixCode prefixCodes)

getTab :: String -> CodeTab
getTab [a] = [(a, [Zero])]
getTab word = zip frequencies allCodes
    where frequencies = orderByFrequency word
          freqLength = length frequencies
          allCodes = (take (length frequencies - 1) prefixCodes) ++ [(replicate (freqLength - 1) One)]

lookupCode :: CodeTab -> Char -> Code
lookupCode tab char = snd $ head $ filter ((==char) . fst) tab

encode :: String -> (CodeTab,Code)
encode word = (tab, concat $ map (lookupCode tab) word)
    where tab = getTab word

lookupPrefix :: (CodeTab,Code) -> TabEntry
lookupPrefix (tab, code) = head $ filter f tab
    where f tabEntry = ((take (length $ snd tabEntry) code)==(snd tabEntry))

decode :: (CodeTab,Code) -> String
decode (tab, []) = []
decode t@(tab, code) = (fst lookedUp):(decode (tab, drop (length $ snd lookedUp) code))
    where lookedUp = lookupPrefix t