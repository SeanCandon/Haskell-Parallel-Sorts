module Sorting
where
import Control.DeepSeq
import Control.Parallel

quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (x:xs) = left ++ x : right
                    where
                    left = quicksort[y | y <- xs, y < x]
                    right = quicksort[y | y <- xs, y >= x]

parQuicksort :: [Integer] -> [Integer]
parQuicksort [] = []
parQuicksort (x:xs) = par left (pseq right (left ++ x : right))
                    where
                      left = parQuicksort [y | y <- xs, y < x]
                      right = parQuicksort [y | y <- xs, y >= x]

parQuicksort2 :: Integer -> [Integer] -> [Integer]
parQuicksort2 _ [] = []
parQuicksort2 0 xs = quicksort xs
parQuicksort2 n (x:xs) = par left (pseq right (left ++ x : right))
                    where
                      left = parQuicksort2 (n-1) [y | y <- xs, y < x]
                      right = parQuicksort2 (n-1) [y | y <- xs, y >= x]

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  merge (mergeSort left) (mergeSort right)
    where left = take ((length xs) `div` 2) xs
          right = drop ((length xs) `div` 2) xs

-- Expects a and b to already be sorted
merge :: [Integer] -> [Integer] -> [Integer]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x < y     = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)

parmergeSort :: [Integer] -> [Integer]
parmergeSort [] = []
parmergeSort [x] = [x]
parmergeSort xs =
  par left (pseq right (merge (parmergeSort left) (parmergeSort right)))
    where left = take ((length xs) `div` 2) xs
          right = drop ((length xs) `div` 2) xs
