{-# LANGUAGE OverloadedStrings #-}

module Test.OneWaySwap where

import Test.Tasty

import Test.OneWaySwap.CreateSwap as CreateSwap
import Test.OneWaySwap.CloseOrUpdate as CloseOrUpdate
import Test.OneWaySwap.Swap as Swap

tests :: TestTree
tests = testGroup "One-Way Swaps"
  [ CreateSwap.tests
  , CloseOrUpdate.tests
  , Swap.tests
  ]
