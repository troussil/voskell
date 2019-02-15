module Main where

import TestKernel
import System.Exit

main :: IO ()
main = do
  let tests = [runTestKernel]
  good <- fmap and (sequence tests)
  if good
     then exitSuccess
     else exitFailure
