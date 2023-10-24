{-# LANGUAGE OverloadedStrings #-}

module Test.OneWaySwap where

import Test.Tasty

import qualified Test.OneWaySwap.CreateSwap as CreateSwap
import qualified Test.OneWaySwap.CloseOrUpdate as CloseOrUpdate
import qualified Test.OneWaySwap.Swap as Swap

tests :: TestTree
tests = testGroup "One-Way Swaps"
  [ CreateSwap.tests
  , CloseOrUpdate.tests
  , Swap.tests
  ]
