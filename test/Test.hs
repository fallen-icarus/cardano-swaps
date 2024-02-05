{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty

import Test.OneWaySwap as OneWaySwap
import Test.TwoWaySwap as TwoWaySwap

main :: IO ()
main = do
  defaultMain $ testGroup "Cardano-Swaps"
    [ OneWaySwap.tests
    , TwoWaySwap.tests
    ]
