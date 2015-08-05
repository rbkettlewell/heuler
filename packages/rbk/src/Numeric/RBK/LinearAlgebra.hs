module Numeric.RBK.LinearAlgebra (
  toUpperTriangular,
  dotVector,
  multiplyMM,
  multiplyMV,
  solveX,
  transpose
  )where

absFractional :: (Ord a, Fractional a) => a -> a
absFractional x = if x < 0.0 then -1.0 * x else x

appendEndColumn :: (Num a, Ord a, Fractional a) => [[a]] -> [a] -> [[a]]
appendEndColumn a b = map (\x -> fst x ++ [snd x]) $ zip a b

backSubstitution :: (Num a, Fractional a) => [a] -> [[a]] -> [a]
backSubstitution solved (u:us)
  | null us = [x]
  | otherwise = x : backSubstitution xSolved us
  where
    ci = last u
    uTail = tail u
    cX = head u
    cKnown = sum . zipWith ((*) . ((-1)*)) solved  $ (init . tail) u
    x = (ci + cKnown) / cX
    xSolved = x : solved

eliminateMatrix :: (Num a, Ord a, Fractional a) => [[a]] -> [[a]]
eliminateMatrix (r:rs)
  | null r = []
  | null rs = []
  | otherwise = (tail . head) eliminate : eliminateMatrix (getSubMatrix eliminate)
  where
    eliminate = map (\ri -> subtractRowVectors ri  $ scalePivotRow $ head ri) rs
    pivot = head r
    sign = getSign pivot
    mult =  absFractional . (/pivot)
    scalePivotRow m  = scaleRowVector r (sign m * mult m)

getSign :: (Ord a, Fractional a) => a -> a -> a
getSign piv fac
    | (piv * fac) < 0.0 = if piv < 0.0 then 1.0 else -1.0
    | otherwise = 1.0

getSubMatrix :: (Num a, Ord a, Fractional a) => [[a]] -> [[a]]
getSubMatrix = map tail

splitFirstCol :: (Num a, Fractional a) => [[a]] -> ([a],[[a]])
splitFirstCol (x:xs)
  | null xs = ([fCol],fTail)
  | otherwise = (\(fC,fT)-> ((fCol:fC),(fTail ++ fT))) $ splitFirstCol xs
    where
      fCol = head x
      fTail = [tail x]

transpose :: (Num a, Fractional a) => [[a]] -> [[a]]
transpose a
  | (length $ (head a)) < 2 = [firstCol] -- cannot split a first column from itself
  | otherwise = firstCol : transpose rest
    where
    (firstCol,rest) = splitFirstCol a

dotVector :: (Num a, Fractional a) => [a] -> [a] -> a
dotVector x y = sum $ zipWith (*) x y

multiplyMV :: (Num a, Fractional a)=> [[a]] -> [a] -> [a]
multiplyMV a v = map (dotVector v) a

multiplyMM :: (Num a, Fractional a)=> [[a]] -> [[a]] -> [[a]]
multiplyMM a b = map (multiplyMV a) b

scaleRowVector :: (Fractional a) => [a] -> a -> [a]
scaleRowVector ri m = map (*m) ri

solveX :: (Num a, Ord a, Fractional a) => [[a]] -> [a] -> [a]
solveX a b = reverse $ backSubstitution [] uReversed
    where
        uReversed = reverse $ toUpperTriangular a b

subtractRowVectors:: (Fractional a) => [a] -> [a] -> [a]
subtractRowVectors = zipWith (-)

toUpperTriangular :: (Num a, Ord a, Fractional a) => [[a]] -> [a] -> [[a]]
toUpperTriangular a b =  firstRow : eliminateMatrix (appendEndColumn a b)
    where firstRow = head a ++ [head b]
