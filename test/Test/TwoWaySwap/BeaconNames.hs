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

import CardanoSwaps.TwoWaySwap
import CardanoSwaps.Utils

-- | The reverse direction of a swap yields the same pair beacon name.
uniquenessTest1 :: TestTree
uniquenessTest1 = 
  testCase "uniquenessTest1" $ assertBool "Fail TwoWaySwap.uniquenessTest1" $
    genTwoWayPairBeaconName testToken1 (adaSymbol,adaToken) ==
      genTwoWayPairBeaconName (adaSymbol,adaToken) testToken1
    
-- | The offer beacon is different than the trading pair beacon.
uniquenessTest2 :: TestTree
uniquenessTest2 = 
  testCase "uniquenessTest2" $ assertBool "Fail TwoWaySwap.uniquenessTest2" $
    genTwoWayPairBeaconName testToken1 (adaSymbol,adaToken) /= 
      genAssetBeaconName testToken1 &&
    genTwoWayPairBeaconName testToken1 (adaSymbol,adaToken) /= 
      genAssetBeaconName (adaSymbol,adaToken) &&
    genTwoWayPairBeaconName (adaSymbol,adaToken) testToken1 /= 
      genAssetBeaconName testToken1 &&
    genTwoWayPairBeaconName (adaSymbol,adaToken) testToken1 /= 
      genAssetBeaconName (adaSymbol,adaToken)

tests :: TestTree
tests = testGroup "Beacon Names"
  [ uniquenessTest1
  , uniquenessTest2
  ]
