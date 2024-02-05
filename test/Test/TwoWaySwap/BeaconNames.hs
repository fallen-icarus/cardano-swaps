{-# LANGUAGE OverloadedStrings #-}

module Test.TwoWaySwap.BeaconNames 
  (
    uniquenessTest1
  , uniquenessTest2

  , tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import CardanoSwaps.TwoWaySwap
import CardanoSwaps.Utils

import Test.Prelude (testTokenSymbol)

testToken1 :: (CurrencySymbol,TokenName)
testToken1 = (testTokenSymbol,"TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = (testTokenSymbol,"TestToken2")

-- | The reverse direction of a swap yields the same pair beacon name.
uniquenessTest1 :: TestTree
uniquenessTest1 = 
  testCase "uniquenessTest1" $ assertBool "Fail TwoWaySwap.uniquenessTest1" $
    genPairBeaconName testToken1 (adaSymbol,adaToken) ==
      genPairBeaconName (adaSymbol,adaToken) testToken1
    
-- | The asset beacons are different than the trading pair beacon.
uniquenessTest2 :: TestTree
uniquenessTest2 = 
  testCase "uniquenessTest2" $ assertBool "Fail TwoWaySwap.uniquenessTest2" $
    genPairBeaconName testToken1 (adaSymbol,adaToken) /= genAssetBeaconName testToken1 &&
    genPairBeaconName testToken1 (adaSymbol,adaToken) /= genAssetBeaconName (adaSymbol,adaToken) &&
    genPairBeaconName (adaSymbol,adaToken) testToken1 /= genAssetBeaconName testToken1 &&
    genPairBeaconName (adaSymbol,adaToken) testToken1 /= genAssetBeaconName (adaSymbol,adaToken)

tests :: TestTree
tests = testGroup "Beacon Names"
  [ uniquenessTest1
  , uniquenessTest2
  ]
