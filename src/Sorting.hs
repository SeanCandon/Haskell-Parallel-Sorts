module Sorting
where
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (x:xs) = left ++ x : right
                    where
                    left = quicksort[y | y <- xs, y < x]
                    right = quicksort[y | y <- xs, y >= x]

parQuicksort :: [Integer] -> [Integer]
parQuicksort [] = []
parQuicksort (x:xs) = left ++ x : right
                        where
                          left = runEval $ rpar $ parQuicksort [y | y <- xs, y < x]
                          right = runEval $ rpar $ parQuicksort [y | y <- xs, y >= x]

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  merge (mergeSort left) (mergeSort right)
    where left = take ((length xs) `div` 2) xs
          right = drop ((length xs) `div` 2) xs

merge :: [Integer] -> [Integer] -> [Integer]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x < y     = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)

parMergeSort :: [Integer] -> [Integer]
parMergeSort [] = []
parMergeSort [x] = [x]
parMergeSort xs = runEval $ do
                      l <- rpar left
                      r <- rpar right
                      return $ merge l r
                      where
                        left = parMergeSort $ take ((length xs) `div` 2) xs
                        right = parMergeSort $ drop ((length xs) `div` 2) xs
