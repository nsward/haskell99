----- Problems 31 - 41: Arithmetic -----

module Arithmetic where
import Data.List

----- 31: Determine whether a given integer number is prime.
-- λ> isPrime 7
-- True

root :: Integral a => a -> a
root = floor . sqrt . fromIntegral

-- check all numbers from 2 to n-1
isPrimeAll :: Integral a => a -> Bool
isPrimeAll k = all ((/=0).mod k) [2..(k-1)]

-- check all numbers from 2 to n/2
isPrimeHalf :: Integral a => a -> Bool
isPrimeHalf k = all ((/=0).mod k) [2..(div k 2)]

-- check all numbers from 2 to sqrt n
isPrimeSqrt :: Integral a => a -> Bool
isPrimeSqrt k = all ((/=0).mod k) [2..(root k)]

-- check 2 and all odd numbers from 3 to sqrt n
isPrimeOdd :: Integral a => a -> Bool
isPrimeOdd k = all ((/=0).mod k) (2:[3,5..(root k)])

-- check all primes from 2 to sqrt n using `primes`
isPrimePrimes :: Integral a => a -> Bool
isPrimePrimes k = all ((/=0).mod k) (primes [2..(root k)])

isPrimeC1 :: (Integral a) => a -> Bool
isPrimeC1 n | n < 4 = n > 1
isPrimeC1 n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
  where s = floor $ sqrt $ fromIntegral n

isPrimeC2 n | n < 4 = n /= 1
isPrimeC2 n = all ((/=0) . mod n) $ takeWhile (<= m) candidates
  where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
        m          = floor . sqrt $ fromIntegral n

-- find all primes in a given range
primes :: Integral a => [a] -> [a]
primes [] = []
primes (p:xs) = p : primes [x | x <- xs, x `mod` p > 0]

primes2 :: Integral a => [a] -> [a]
primes2 [x] = [x]
primes2 (p:xs) = p : primes2 left
  where left = xs \\ crossed_out
        crossed_out = takeWhile (<= last xs) [p*i | i <- [2..]]


----- 32: Determine the greatest common divisor of two positive integer numbers using Euclid's algorithm.
-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
eucgcd :: Integral a => a -> a -> a
-- naive implementation
eucgcd n m = last [x | x <- [1..(max n m)], n `mod` x == 0, m `mod` x == 0]

----- 33: Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
-- λ> coprime 35 64
-- True
