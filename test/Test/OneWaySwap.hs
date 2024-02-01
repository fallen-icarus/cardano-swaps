{-# LANGUAGE OverloadedStrings #-}

module Test.OneWaySwap where

import Test.Tasty

import qualified Test.OneWaySwap.CreateSwap as CreateSwap
import qualified Test.OneWaySwap.UpdateSwap as UpdateSwap
import qualified Test.OneWaySwap.CloseSwap as CloseSwap
import qualified Test.OneWaySwap.Swap as Swap
import qualified Test.OneWaySwap.BeaconNames as BeaconNames

tests :: TestTree
tests = testGroup "One-Way Swaps"
  [ CreateSwap.tests
  , CloseSwap.tests
  , UpdateSwap.tests
  , Swap.tests
  , BeaconNames.tests
  ]
