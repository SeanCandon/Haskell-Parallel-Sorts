module Main where

import Sorting

import Control.DeepSeq
import System.Random

main :: IO ()
main = do
      input <- sequence $ replicate 100000 $ randomRIO (1,100::Integer)
      a <- (parQuicksort input) `deepseq` (print "OK")
      print "Done"
