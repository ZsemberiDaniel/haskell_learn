import Data.Bits (xor)
import Data.Char (ord, isLetter, toUpper)

data BitVector = BV [Int]
  deriving (Show,Eq)

data BloomFilter = BLF BitVector
  deriving (Show,Eq)

additiveHash :: Int -> [Int] -> Int
additiveHash prime = (`mod` prime) . sum

pearsonHash :: [Int] -> [Int] -> Int
pearsonHash t = foldl (\h cc -> t !! (h `xor` cc)) 0

hashes :: [Int] -> [Int]
hashes list = [additiveHash 191 list, pearsonHash [255,254..0] list]

empty :: BitVector
empty = BV []

add :: BitVector -> Int -> BitVector
add (BV vector) bit = BV $ bit:vector

toBloom :: BitVector -> BloomFilter
toBloom = BLF

bloomAdd :: BloomFilter -> [Int] -> BloomFilter
bloomAdd (BLF bv) list = BLF $ add (add bv (last hs)) (head hs)
    where hs = hashes list

has :: BitVector -> Int -> Bool
has (BV list) = (`elem` list)

bloomQuery :: BloomFilter -> [Int] -> Bool
bloomQuery (BLF bv) = all (has bv) . hashes

normalize :: String -> [Int]
normalize = map (ord . toUpper) . filter isLetter
