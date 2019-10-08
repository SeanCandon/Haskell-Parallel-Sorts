{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.Hspec

import System.Random
import Data.Sort

import Sorting

main :: IO ()
main = run

run = do
        three <- sequence $ replicate 3 $ randomRIO (1,100::Integer)
        ten <- sequence $ replicate 10 $ randomRIO (1,100::Integer)
        hundred <- sequence $ replicate 100 $ randomRIO (1,100::Integer)
        thousand <- sequence $ replicate 1000 $ randomRIO (1,100::Integer)
        tenthousand <- sequence $ replicate 10000 $ randomRIO (1,100::Integer)
        runTests three ten hundred thousand tenthousand
                    -- time
runTests :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> IO ()
runTests three ten hundred thousand tenthousand = hspec $ do
  describe "sort list size 3" $ do
    it "quicksort" $ do
      quicksort three `shouldBe` sort three
    it "parallel quicksort" $ do
      parQuicksort three `shouldBe` sort three
    it "mergesort" $ do
      mergeSort three `shouldBe` sort three
    it "parallel mergesort" $ do
      parmergeSort three `shouldBe` sort three

  describe "sort list size 10" $ do
    it "quicksort" $ do
      quicksort ten `shouldBe` sort ten
    it "parallel quicksort" $ do
      parQuicksort ten `shouldBe` sort ten
    it "mergesort" $ do
      mergeSort ten `shouldBe` sort ten
    it "parallel mergesort" $ do
      parmergeSort ten `shouldBe` sort ten

  describe "sort list size 100" $ do
    it "quicksort" $ do
      quicksort hundred `shouldBe` sort hundred
    it "parallel quicksort" $ do
      parQuicksort hundred `shouldBe` sort hundred
    it "mergesort" $ do
      mergeSort hundred `shouldBe` sort hundred
    it "parallel mergesort" $ do
      parmergeSort hundred `shouldBe` sort hundred

  describe "sort list size 1000" $ do
    it "quicksort" $ do
      quicksort thousand `shouldBe` sort thousand
    it "parallel quicksort" $ do
      parQuicksort thousand `shouldBe` sort thousand
    it "mergesort" $ do
      mergeSort thousand `shouldBe` sort thousand
    it "parallel mergesort" $ do
      parmergeSort thousand `shouldBe` sort thousand

  describe "sort list size 10000" $ do
    it "quicksort" $ do
      quicksort tenthousand `shouldBe` sort tenthousand
    it "parallel quicksort" $ do
      parQuicksort tenthousand `shouldBe` sort tenthousand
    it "mergesort" $ do
      mergeSort tenthousand `shouldBe` sort tenthousand
    it "parallel mergesort" $ do
      parmergeSort tenthousand `shouldBe` sort tenthousand
