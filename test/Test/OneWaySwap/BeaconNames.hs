{-# LANGUAGE OverloadedStrings #-}

module Test.OneWaySwap.BeaconNames 
  (
    uniquenessTest1
  , uniquenessTest2
  , uniquenessTest3
  , uniquenessTest4
  , uniquenessTest5

  , tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import CardanoSwaps.OneWaySwap
import CardanoSwaps.Utils

import Test.Prelude (testTokenSymbol)

testToken1 :: (CurrencySymbol,TokenName)
testToken1 = (testTokenSymbol,"TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = (testTokenSymbol,"TestToken2")

-- | The reverse direction of a swap yields a different pair beacon name.
uniquenessTest1 :: TestTree
uniquenessTest1 = 
  testCase "uniquenessTest1" $ assertBool "Fail OneWaySwap.uniquenessTest1" $
    genPairBeaconName (OfferAsset testToken1) (AskAsset (adaSymbol,adaToken)) /=
      genPairBeaconName (OfferAsset (adaSymbol,adaToken)) (AskAsset testToken1)
    
-- | The offer beacon is different than the trading pair beacon.
uniquenessTest2 :: TestTree
uniquenessTest2 = 
  testCase "uniquenessTest2" $ assertBool "Fail OneWaySwap.uniquenessTest2" $
    genPairBeaconName (OfferAsset testToken1) (AskAsset (adaSymbol,adaToken)) /=
      genOfferBeaconName (OfferAsset testToken1) &&

    genPairBeaconName (OfferAsset testToken1) (AskAsset (adaSymbol,adaToken)) /=
      genOfferBeaconName (OfferAsset (adaSymbol,adaToken)) &&

    genPairBeaconName (OfferAsset (adaSymbol,adaToken)) (AskAsset testToken1) /= 
      genOfferBeaconName (OfferAsset testToken1) &&

    genPairBeaconName (OfferAsset (adaSymbol,adaToken)) (AskAsset testToken1) /= 
      genOfferBeaconName (OfferAsset (adaSymbol,adaToken))

-- | The ask beacon is different than the trading pair beacon.
uniquenessTest3 :: TestTree
uniquenessTest3 = 
  testCase "uniquenessTest3" $ assertBool "Fail OneWaySwap.uniquenessTest3" $
    genPairBeaconName (OfferAsset testToken1) (AskAsset (adaSymbol,adaToken)) /=
      genAskBeaconName (AskAsset testToken1) &&

    genPairBeaconName (OfferAsset testToken1) (AskAsset (adaSymbol,adaToken)) /=
      genAskBeaconName (AskAsset (adaSymbol,adaToken)) &&

    genPairBeaconName (OfferAsset (adaSymbol,adaToken)) (AskAsset testToken1) /= 
      genAskBeaconName (AskAsset testToken1) &&

    genPairBeaconName (OfferAsset (adaSymbol,adaToken)) (AskAsset testToken1) /= 
      genAskBeaconName (AskAsset (adaSymbol,adaToken))

-- | The ask beacon is different than the offer beacon.
uniquenessTest4 :: TestTree
uniquenessTest4 = 
  testCase "uniquenessTest4" $ assertBool "Fail OneWaySwap.uniquenessTest4" $
    genOfferBeaconName (OfferAsset testToken1) /= 
      genAskBeaconName (AskAsset testToken1) &&

    genOfferBeaconName (OfferAsset (adaSymbol,adaToken)) /= 
      genAskBeaconName (AskAsset (adaSymbol,adaToken))

-- | Two assets have different offer and ask beacons.
uniquenessTest5 :: TestTree
uniquenessTest5 =
  testCase "uniquenessTest5" $ assertBool "Fail OneWaySwap.uniquenessTest5" $
    genOfferBeaconName (OfferAsset testToken1) /= 
      genOfferBeaconName (OfferAsset (adaSymbol,adaToken)) &&

    genAskBeaconName (AskAsset testToken1) /= 
      genAskBeaconName (AskAsset (adaSymbol,adaToken)) &&

    genOfferBeaconName (OfferAsset testToken1) /= 
      genOfferBeaconName (OfferAsset testToken2) &&

    genAskBeaconName (AskAsset testToken1) /= 
      genAskBeaconName (AskAsset testToken2)

tests :: TestTree
tests = testGroup "Beacon Names"
  [ uniquenessTest1
  , uniquenessTest2
  , uniquenessTest3
  , uniquenessTest4
  , uniquenessTest5
  ]
