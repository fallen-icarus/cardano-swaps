{-# LANGUAGE OverloadedStrings #-}

module Test.TwoWaySwap where

import Test.Tasty

import Test.TwoWaySwap.CreateSwap as CreateSwap
import Test.TwoWaySwap.CloseOrUpdate as CloseOrUpdate
import Test.TwoWaySwap.Swap as Swap

tests :: TestTree
tests = testGroup "Two-Way Swaps"
  [ CreateSwap.tests
  , CloseOrUpdate.tests
  , Swap.tests
  ]
