module Main where

import Criterion.Main
import Arithmetic

main = defaultMain
  -- (defaultConfig {reportFile = Just "bench.html"})
  [bgroup "primality check"
    [ bench "all"    $ nf isPrimeAll k
    , bench "half"   $ nf isPrimeHalf k
    , bench "sqrt"   $ nf isPrimeSqrt k
    , bench "odd"    $ nf isPrimeOdd k
    , bench "prime"  $ nf isPrimePrimes k
    , bench "C1"     $ nf isPrimeC1 k
    , bench "C2"     $ nf isPrimeC2 k]
  ,bgroup "get primes"
    [ bench "primes" $ nf primes primeRange
    , bench "primes2" $ nf primes2 primeRange]]

k :: Int
-- n = 7919
k = 99371
primeRange :: [Int]
-- primeRange = [2..10000]
primeRange = [2..(root k)]
