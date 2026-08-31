{-# LANGUAGE OverloadedStrings #-}

module Test.OneWaySwap where

import Test.Tasty
import Test.Tasty.HUnit

import CardanoSwaps.OneWaySwap

import qualified Test.OneWaySwap.CreateSwap as CreateSwap
import qualified Test.OneWaySwap.UpdateSwap as UpdateSwap
import qualified Test.OneWaySwap.CloseSwap as CloseSwap
import qualified Test.OneWaySwap.Swap as Swap
import qualified Test.OneWaySwap.BeaconNames as BeaconNames
import qualified Test.OneWaySwap.Publish as Publish

tests :: TestTree
tests = testGroup "One-Way Swaps"
  [ CreateSwap.tests
  , CloseSwap.tests
  , UpdateSwap.tests
  , Swap.tests
  , BeaconNames.tests
  , Publish.tests
    -- The script sizes impact user fees. The beacon script size includes the applied swap
    -- validator hash.
  , testGroup "Script Sizes"
      [ testCase "swapScript size" $ swapScriptSize @?= 3490
      , testCase "beaconScript size" $ beaconScriptSize @?= 3759
      ]
  ]
