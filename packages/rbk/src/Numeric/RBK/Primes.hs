module Numeric.RBK.Primes
(
isPrime,
primeFactors,
sieve,
Primes
)where

import qualified Data.List as List

type Primes = [Int]

checkPrime :: Int -> Primes -> Bool
checkPrime = List.elem

isPrime :: Int -> Primes -> Bool
isPrime x primes
    | x == 1 = False
    | List.elem x primes = True
    | otherwise = null . dropWhile (\p-> x `mod` p /= 0 && p <= (div x 2)) $ primes

primeFactors :: Int -> Primes -> Primes
primeFactors x primes
  | checkPrime x primes = [x]
  | otherwise        = firstDivisor : primeFactors (x `div` firstDivisor) primes
  where firstDivisor = head . dropWhile (\p-> x `mod` p /= 0 && p <= (div x 2)) $ primes

sieve :: Int -> Primes
sieve n = fst $ sieveOfEratos ([],[2..n])

sieveOfEratos :: (Primes,[Int]) -> (Primes,[Int])
sieveOfEratos (xs,[])     = (xs,[])
sieveOfEratos (xs,allY@(y:ys)) = sieveOfEratos (xs ++ [y], allY List.\\ multiplicands)
  where multiplicands = takeWhile (<= last allY) [y,y*2..]
