{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty

import Test.OneWaySwap as OneWaySwap
import Test.TwoWaySwap as TwoWaySwap

{- | Tests are broken into 5 categories:

(1) Regression Tests - tests that prove a certain feature actually works.

(2) Failure Tests - tests that prove a certain action will fail.

(3) Edge Case Tests - tests of unusual usages that should or should not fail.

(4) Benchmark Tests - tests that will fail if the performance of the protocol decreases.

(5) Performance Increase Tests - tests that will fail if the performance of the protocol increases.

-}
main :: IO ()
main = do
  defaultMain $ testGroup "Cardano-Swaps"
    [ 
      OneWaySwap.tests
    , TwoWaySwap.tests
    ]
