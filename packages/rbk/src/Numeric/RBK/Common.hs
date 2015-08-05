module Numeric.RBK.Common
(
binaryListToInt,
intToBinaryString,
intToBinaryList,
isPalindrome,
rotations,
splitDigits,
smushDigits,
)where

import Data.Char
import Numeric (showIntAtBase)

ror :: Int -> [Int] -> [[Int]]
ror r xAll@(x:xs)
  | r <= 0 = [xAll]
  | otherwise = xAll : ror (r-1) y
  where y = xs ++ [x]

rotations :: Int -> Int -> [Int]
rotations r x = map smushDigits $ ror r $ splitDigits x

splitDigits :: Int -> [Int]
splitDigits n = map digitToInt $ show n

smushDigits :: [Int] -> Int
smushDigits = read . concat . map show

intToBinaryString :: Int -> String
intToBinaryString x = showIntAtBase 2 intToDigit x ""

intToBinaryList :: Int -> [Int]
intToBinaryList x = map digitToInt $ intToBinaryString x

binaryListToInt :: [Int] -> Int
binaryListToInt bL = sum $ zipWith (*) bL $ reverse [2^x | x<-[0..]]

isPalindrome :: [Int] -> Bool
isPalindrome x
  | null x = True
  | null (tail x) = True
  | otherwise = (head x  == last x) && isPalindrome rest
  where
    rest = (tail . init) x

