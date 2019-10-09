module Main where

import Criterion.Main

import System.Random

import Sorting

main = run

run :: IO ()
run = do
        three <- sequence $ replicate 3 $ randomRIO (1,100::Integer)
        ten <- sequence $ replicate 10 $ randomRIO (1,100::Integer)
        hundred <- sequence $ replicate 100 $ randomRIO (1,100::Integer)
        thousand <- sequence $ replicate 1000 $ randomRIO (1,100::Integer)
        tenthousand <- sequence $ replicate 10000 $ randomRIO (1,100::Integer)
        defaultMain $ runBenches three ten hundred thousand tenthousand

runBenches :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer]-> [Benchmark]
runBenches three ten hundred thousand tenthousand =
                    [ bgroup "Quicksort"
                      [ bench "3"
                        (whnf quicksort three),
                        bench "10"
                        (whnf quicksort ten),
                        bench "100"
                        (whnf quicksort hundred),
                        bench "1000"
                        (whnf quicksort thousand),
                        bench "10000"
                        (whnf quicksort tenthousand)
                      ],
                      bgroup "Parallel Quicksort"
                      [ bench "3"
                        (whnf parQuicksort3 three),
                        bench "10"
                        (whnf parQuicksort3 ten),
                        bench "100"
                        (whnf parQuicksort3 hundred),
                        bench "1000"
                        (whnf parQuicksort3 thousand),
                        bench "10000"
                        (whnf parQuicksort3 tenthousand)
                      ],
                      bgroup "Mergesort"
                      [ bench "3"
                        (whnf mergeSort three),
                        bench "10"
                        (whnf mergeSort ten),
                        bench "100"
                        (whnf mergeSort hundred),
                        bench "1000"
                        (whnf mergeSort thousand),
                        bench "10000"
                        (whnf mergeSort tenthousand)
                      ],
                      bgroup "Parallel Mergesort"
                      [ bench "3"
                        (whnf parmergeSort3 three),
                        bench "10"
                        (whnf parmergeSort3 ten),
                        bench "100"
                        (whnf parmergeSort3 hundred),
                        bench "1000"
                        (whnf parmergeSort3 thousand),
                        bench "10000"
                        (whnf parmergeSort3 tenthousand)
                      ]
                    ]
