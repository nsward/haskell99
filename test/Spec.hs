module Main where

import Test.Hspec
import Arithmetic
import Constants

main :: IO ()
main = hspec $ do

  describe "get primes" $ do
    let test_case = [2..500]
    let should = primes_to_500
    describe "primes" $ do
      it "works" $ do
       let is = primes test_case
       is `shouldBe` should
    describe "primes2" $ do
      it "works" $ do
       let is = primes2 test_case
       is `shouldBe` should

    -- describe "sieve" $ do
    --   it "works" $ do
    --    let is = primes3 test_case
    --    is `shouldBe` should

  describe "check primality" $ do
    let test_cases = [7, 9]
    let should = [True, False]
    describe "isPrimeAll" $ do
      it "works" $ do
        let is = map isPrimeAll test_cases
        is `shouldBe` should
    describe "isPrimeHalf" $ do
      it "works" $ do
        let is = map isPrimeHalf test_cases
        is `shouldBe` should
    describe "isPrimeSqrt" $ do
      it "works" $ do
        let is = map isPrimeSqrt test_cases
        is `shouldBe` should
    describe "isPrimeOdd" $ do
      it "works" $ do
        let is = map isPrimeOdd test_cases
        is `shouldBe` should
    describe "isPrimePrimes" $ do
      it "works" $ do
        let is = map isPrimePrimes test_cases
        is `shouldBe` should
    describe "isPrimePrimes" $ do
      it "works" $ do
        let is = map isPrimePrimes test_cases
        is `shouldBe` should
    describe "isPrimeC1" $ do
      it "works" $ do
        let is = map isPrimeC1 test_cases
        is `shouldBe` should
    describe "isPrimeC2" $ do
      it "works" $ do
        let is = map isPrimeC2 test_cases
        is `shouldBe` should
