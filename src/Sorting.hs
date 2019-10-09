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
parQuicksort (x:xs) = par left (pseq right (left ++ x : right))
                    where
                      left = parQuicksort [y | y <- xs, y < x]
                      right = parQuicksort [y | y <- xs, y >= x]

parQuicksort2 :: (Integer,[Integer]) -> [Integer]
parQuicksort2 (_,[]) = []
parQuicksort2 (0,xs) = quicksort xs
parQuicksort2 (n,(x:xs)) = par left (pseq right (left ++ x : right))
                    where
                      left = parQuicksort2 ((n-1),[y | y <- xs, y < x])
                      right = parQuicksort2 ((n-1),[y | y <- xs, y >= x])

parQuicksort3 :: [Integer] -> Eval [Integer]
parQuicksort3 [] = return []
parQuicksort3 (x:xs) = do
                        left <- rpar $ runEval $ parQuicksort3 [y | y <- xs, y < x]
                        right <- rpar $ runEval $ parQuicksort3 [y | y <- xs, y >= x]
                        rseq $ left ++ x : right

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

parmergeSort :: [Integer] -> [Integer]
parmergeSort [] = []
parmergeSort [x] = [x]
parmergeSort xs =
  par left (pseq right (merge (parmergeSort left) (parmergeSort right)))
    where left = take ((length xs) `div` 2) xs
          right = drop ((length xs) `div` 2) xs

parmergeSort2 :: (Integer,[Integer]) -> [Integer]
parmergeSort2 (_,[]) = []
parmergeSort2 (_,[x]) = [x]
parmergeSort2 (0,xs) = parmergeSort xs
parmergeSort2 (n,xs) =
  par left (pseq right (merge (parmergeSort2 ((n-1),left)) (parmergeSort2 ((n-1),right))))
    where left = take ((length xs) `div` 2) xs
          right = drop ((length xs) `div` 2) xs

parmergeSort3 :: [Integer] -> Eval [Integer]
parmergeSort3 [] = return []
parmergeSort3 [x] = return [x]
parmergeSort3 xs = do
                    xr <- rpar $ runEval $ parmergeSort3 $ take ((length xs) `div` 2) xs
                    yr <- rseq $ runEval $ parmergeSort3 $ drop ((length xs) `div` 2) xs
                    rseq (merge xr yr)

fib :: Integer -> Integer
fib n | n < 2 = 1
fib n = fib (n-1) + fib (n-2)

fibpar :: Integer -> Integer
fibpar n | n < 2 = 1
fibpar n = par nf1 (pseq nf2 (nf1 + nf2))
              where nf1 = fibpar (n-1)
                    nf2 = fibpar (n-2)
