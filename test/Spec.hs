{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.Hspec

import System.Random
import Data.Sort

import Sorting

main :: IO ()
main = defaultTestFunc

defaultTestFunc = do
                    runHspecUnitTests
                    time

runHspecUnitTests = hspec $ do
  describe "quicksort tests" $ do
    it "sort list size 3" $ do
      testsort 3 quicksort
    it "sort list size 10" $ do
      testsort 10 quicksort
    it "sort list size 100" $ do
      testsort 100 quicksort
    it "sort list size 1000" $ do
      testsort 1000 quicksort
    it "sort list size 10000" $ do
      testsort 10000 quicksort

  describe "parallel quicksort tests" $ do
    it "sort list size 3" $ do
      testsort 3 parQuicksort
    it "sort list size 10" $ do
      testsort 10 parQuicksort
    it "sort list size 100" $ do
      testsort 100 parQuicksort
    it "sort list size 1000" $ do
      testsort 1000 parQuicksort
    it "sort list size 10000" $ do
      testsort 10000 parQuicksort

  describe "mergesort tests" $ do
    it "sort list size 3" $ do
      testsort 3 mergeSort
    it "sort list size 10" $ do
      testsort 10 mergeSort
    it "sort list size 100" $ do
      testsort 100 mergeSort
    it "sort list size 1000" $ do
      testsort 1000 mergeSort
    it "sort list size 10000" $ do
      testsort 10000 mergeSort

  describe "parallel mergesort tests" $ do
    it "sort list size 3" $ do
      testsort 3 parmergeSort
    it "sort list size 10" $ do
      testsort 10 parmergeSort
    it "sort list size 100" $ do
      testsort 100 parmergeSort
    it "sort list size 1000" $ do
      testsort 1000 parmergeSort
    it "sort list size 10000" $ do
      testsort 10000 parmergeSort

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
