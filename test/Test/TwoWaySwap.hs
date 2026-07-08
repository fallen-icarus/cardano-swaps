{-# LANGUAGE OverloadedStrings #-}

module Test.TwoWaySwap where

import Test.Tasty

import qualified Test.TwoWaySwap.CreateSwap as CreateSwap
import qualified Test.TwoWaySwap.UpdateSwap as UpdateSwap
import qualified Test.TwoWaySwap.CloseSwap as CloseSwap
import qualified Test.TwoWaySwap.Swap as Swap
import qualified Test.TwoWaySwap.BeaconNames as BeaconNames
import qualified Test.TwoWaySwap.Publish as Publish

tests :: TestTree
tests = testGroup "Two-Way Swaps"
  [ CreateSwap.tests
  , CloseSwap.tests
  , UpdateSwap.tests
  , Swap.tests
  , BeaconNames.tests
  , Publish.tests
  ]
