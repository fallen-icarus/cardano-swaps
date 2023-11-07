{-# LANGUAGE OverloadedStrings #-}

module Test.TwoWaySwap.BeaconNames 
  (
    uniquenessTest1
  , uniquenessTest2

  , tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Config

import CardanoSwaps.Utils

-- | The reverse direction of a swap yields the same pair beacon name.
uniquenessTest1 :: TestTree
uniquenessTest1 = 
  testCase "uniquenessTest1" $ assertBool "Fail TwoWaySwap.uniquenessTest1" $
    genSortedPairBeaconName testToken1 (adaSymbol,adaToken) ==
      genSortedPairBeaconName (adaSymbol,adaToken) testToken1
    
-- | The offer beacon is different than the trading pair beacon.
uniquenessTest2 :: TestTree
uniquenessTest2 = 
  testCase "uniquenessTest2" $ assertBool "Fail TwoWaySwap.uniquenessTest2" $
    (genSortedPairBeaconName testToken1 (adaSymbol,adaToken) /= 
      uncurry genOfferBeaconName testToken1) &&
    (genSortedPairBeaconName testToken1 (adaSymbol,adaToken) /= 
      genOfferBeaconName adaSymbol adaToken) &&
    (genSortedPairBeaconName (adaSymbol,adaToken) testToken1 /= 
      uncurry genOfferBeaconName testToken1) &&
    (genSortedPairBeaconName (adaSymbol,adaToken) testToken1 /= 
      genOfferBeaconName adaSymbol adaToken)

tests :: TestTree
tests = testGroup "Beacon Names"
  [ uniquenessTest1
  , uniquenessTest2
  ]
