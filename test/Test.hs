module Main where

import Plutus.Contract
import Test.Tasty

import Tests.Swap
import CardanoSwaps

main :: IO ()
main = test -- defaultMain test