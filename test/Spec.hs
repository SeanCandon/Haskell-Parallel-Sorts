{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.Hspec

import System.Random
import Data.Sort

import Sorting

main :: IO ()
main = run

run = do
        g <- getStdGen
        let three = (take 3 (randomRs (1,100) g))::[Integer]
        let ten = (take 10 (randomRs (1,100) g))::[Integer]
        let hundred = (take 100 (randomRs (1,100) g))::[Integer]
        let thousand = (take 1000 (randomRs (1,100) g))::[Integer]
        let tenthousand = (take 10000 (randomRs (1,100) g))::[Integer]
        runTests three ten hundred thousand tenthousand
                    -- time
runTests :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> IO ()
runTests three ten hundred thousand tenthousand = hspec $ do
  describe "quicksort tests" $ do
    it "sort list size 3" $ do
      quicksort three `shouldBe` sort three
    it "sort list size 10" $ do
      quicksort ten `shouldBe` sort ten
    it "sort list size 100" $ do
      quicksort hundred `shouldBe` sort hundred
    it "sort list size 1000" $ do
      quicksort thousand `shouldBe` sort thousand
    it "sort list size 10000" $ do
      quicksort tenthousand `shouldBe` sort tenthousand

  describe "parallel quicksort tests" $ do
    it "sort list size 3" $ do
      parQuicksort three `shouldBe` sort three
    it "sort list size 10" $ do
      parQuicksort ten `shouldBe` sort ten
    it "sort list size 100" $ do
      parQuicksort hundred `shouldBe` sort hundred
    it "sort list size 1000" $ do
      parQuicksort thousand `shouldBe` sort thousand
    it "sort list size 10000" $ do
      parQuicksort tenthousand `shouldBe` sort tenthousand

  describe "mergesort tests" $ do
    it "sort list size 3" $ do
      mergeSort three `shouldBe` sort three
    it "sort list size 10" $ do
      mergeSort ten `shouldBe` sort ten
    it "sort list size 100" $ do
      mergeSort hundred `shouldBe` sort hundred
    it "sort list size 1000" $ do
      mergeSort thousand `shouldBe` sort thousand
    it "sort list size 10000" $ do
      mergeSort tenthousand `shouldBe` sort tenthousand

  describe "parallel mergesort tests" $ do
    it "sort list size 3" $ do
      parmergeSort three `shouldBe` sort three
    it "sort list size 10" $ do
      parmergeSort ten `shouldBe` sort ten
    it "sort list size 100" $ do
      parmergeSort hundred `shouldBe` sort hundred
    it "sort list size 1000" $ do
      parmergeSort thousand `shouldBe` sort thousand
    it "sort list size 10000" $ do
      parmergeSort tenthousand `shouldBe` sort tenthousand

testsort :: Int -> ([Integer] -> [Integer]) -> IO ()
testsort n f = do
                    g <- getStdGen
                    let input = (take n (randomRs (0,100) g))::[Integer]
                    f input `shouldBe` (sort input)


time :: IO ()
time = do
        g <- getStdGen
        let input = (take 20000 (randomRs (0,100) g))::[Integer]
        timed $ quicksort input
        timed $ parQuicksort input
        timed $ mergeSort input
        timed $ parmergeSort input
