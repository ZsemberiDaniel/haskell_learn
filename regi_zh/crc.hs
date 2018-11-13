import Data.Char (ord)
import Data.Bits (xor)

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary nmb = toBinaryRec (last $ takeWhile (\n -> 2 ^ n <= nmb) [0..]) nmb
    where toBinaryRec at n
            | at == -1 = []
            | p <= n = 1:(toBinaryRec (at - 1) (n - p))
            | otherwise = 0:(toBinaryRec (at - 1) n)
                where p = 2 ^ at

chrToBinary :: Char -> [Int]
chrToBinary ch = (replicate (8 - length b) 0) ++ b
    where b = toBinary $ ord ch

strToBinary :: String -> [Int]
strToBinary = foldl (\acc c -> acc ++ chrToBinary c) []

bxor :: [Int] -> [Int] -> [Int]
bxor [] [] = []
bxor (n1:l1) [] = (n1 `xor` 0):(bxor l1 [])
bxor [] (n2:l2) = (0 `xor` n2):(bxor [] l2)
bxor (n1:l1) (n2:l2) = (n1 `xor` n2):(bxor l1 l2)
