module Sorting
where
import Data.Time
import System.CPUTime
import Control.DeepSeq
import Control.Parallel

quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (x:xs) = losort ++ x : hisort
                    where
                    losort = quicksort[y | y <- xs, y < x]
                    hisort = quicksort[y | y <- xs, y >= x]

parQuicksort :: [Integer] -> [Integer]
parQuicksort [] = []
parQuicksort (x:xs) = par losort (pseq hisort (losort ++ x : hisort))
                    where
                      losort = parQuicksort [y | y <- xs, y < x]
                      hisort = parQuicksort [y | y <- xs, y >= x]

parQuicksort2 :: Integer -> [Integer] -> [Integer]
parQuicksort2 _ [] = []
parQuicksort2 0 xs = quicksort xs
parQuicksort2 n (x:xs) = par losort (pseq hisort (losort ++ x : hisort))
                    where
                      losort = parQuicksort2 (n-1) [y | y <- xs, y < x]
                      hisort = parQuicksort2 (n-1) [y | y <- xs, y >= x]

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort a =
  merge (mergeSort firstFew) (mergeSort lastFew)
    where firstFew = take ((length a) `div` 2) a
          lastFew = drop ((length a) `div` 2) a

-- Expects a and b to already be sorted
merge :: [Integer] -> [Integer] -> [Integer]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
  | a < b     = a:(merge as (b:bs))
  | otherwise = b:(merge (a:as) bs)

parmergeSort :: [Integer] -> [Integer]
parmergeSort [] = []
parmergeSort [a] = [a]
parmergeSort a =
  par firstFew (pseq lastFew (merge (parmergeSort firstFew) (parmergeSort lastFew)))
    where firstFew = take ((length a) `div` 2) a
          lastFew = drop ((length a) `div` 2) a

timed :: (NFData a, Show a) => a -> IO ()
timed f = do
                t0 <- getCurrentTime
                let ys = f
                t1 <- ys `deepseq` getCurrentTime
                print $ diffUTCTime t1 t0
