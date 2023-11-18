{-# LANGUAGE OverloadedStrings #-}

module Test.TwoWaySwap where

import Test.Tasty

import qualified Test.TwoWaySwap.CreateSwap as CreateSwap
import qualified Test.TwoWaySwap.CloseOrUpdate as CloseOrUpdate
import qualified Test.TwoWaySwap.Swap as Swap
import qualified Test.TwoWaySwap.BeaconNames as BeaconNames

tests :: TestTree
tests = testGroup "Two-Way Swaps"
  [ CreateSwap.tests
  , CloseOrUpdate.tests
  , Swap.tests
  , BeaconNames.tests
  ]
