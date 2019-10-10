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
        hunthousand <- sequence $ replicate 100000 $ randomRIO (1,100::Integer)
        million <- sequence $ replicate 1000000 $ randomRIO (1,100::Integer)
        defaultMain $ runBenches three ten hundred thousand tenthousand hunthousand million

runBenches :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Benchmark]
runBenches three ten hundred thousand tenthousand hunthousand million =
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
                        (whnf quicksort tenthousand),
                        bench "100000"
                        (whnf quicksort hunthousand),
                        bench "1000000"
                        (whnf quicksort million)
                      ],
                      bgroup "Parallel Quicksort"
                      [ bench "3"
                        (whnf parQuicksort three),
                        bench "10"
                        (whnf parQuicksort ten),
                        bench "100"
                        (whnf parQuicksort hundred),
                        bench "1000"
                        (whnf parQuicksort thousand),
                        bench "10000"
                        (whnf parQuicksort tenthousand),
                        bench "100000"
                        (whnf parQuicksort hunthousand),
                        bench "1000000"
                        (whnf parQuicksort million)
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
                        (whnf mergeSort tenthousand),
                        bench "100000"
                        (whnf mergeSort hunthousand),
                        bench "1000000"
                        (whnf mergeSort million)
                      ],
                      bgroup "Parallel Mergesort"
                      [ bench "3"
                        (whnf parMergeSort three),
                        bench "10"
                        (whnf parMergeSort ten),
                        bench "100"
                        (whnf parMergeSort hundred),
                        bench "1000"
                        (whnf parMergeSort thousand),
                        bench "10000"
                        (whnf parMergeSort tenthousand),
                        bench "100000"
                        (whnf parMergeSort hunthousand),
                        bench "1000000"
                        (whnf parMergeSort million)
                      ]
                    ]
