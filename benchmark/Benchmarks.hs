module Main where

import Criterion.Main

import System.Random

import Sorting

main = run

run :: IO Benchmark
run = do
        g <- getStdGen
        let input1 = (take 100 (randomRs (0,100) g))::[Integer]
        return $ runBenches input1

runBenches :: [Integer] -> Benchmark
runBenches input1 = bgroup "Quicksort1"
              [ bench "100"
                ( whnf quicksort input1 )
              ]
