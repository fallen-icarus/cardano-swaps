{-# LANGUAGE OverloadedStrings #-}

module Test.OneWaySwap.BeaconNames
  (
    uniquenessTest1
  , uniquenessTest2
  , uniquenessTest3
  , uniquenessTest4
  , uniquenessTest5

  , goldenTest1
  , goldenTest2
  , goldenTest3
  , goldenTest4
  , goldenTest5
  , goldenTest6
  , goldenTest7

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

-------------------------------------------------
-- Golden Tests
-------------------------------------------------
-- The expected hashes below are pinned by the golden tests in
-- aiken/lib/cardano_swaps/one_way_swap/utils.ak. Both test suites use the same inputs, so these
-- tests check that the off-chain beacon names match the on-chain ones byte-for-byte. If any of
-- these fail, the off-chain derivation has diverged from the on-chain derivation.

-- | The policy id used by the aiken golden tests (28 bytes).
goldenPolicy :: CurrencySymbol
goldenPolicy = unsafeFromRight $
  readCurrencySymbol "00112233445566778899aabbccddeeff00112233445566778899aabb"

-- | The asset name used by the aiken golden tests ("DJED").
goldenName :: TokenName
goldenName = unsafeFromRight $ readTokenName "444a4544"

-- | A 32-byte asset name (the maximum length).
goldenMaxName :: TokenName
goldenMaxName = unsafeFromRight $
  readTokenName "ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100"

expectedName :: String -> TokenName
expectedName = unsafeFromRight . readTokenName

-- | The offer beacon for ADA.
goldenTest1 :: TestTree
goldenTest1 =
  testCase "goldenTest1" $
    genOfferBeaconName (OfferAsset (adaSymbol,adaToken)) @?=
      expectedName "07d5f63e85046b83e1fc4102a7c19c3f1711c51984725e3b6cf195900947cebe"

-- | The ask beacon for ADA.
goldenTest2 :: TestTree
goldenTest2 =
  testCase "goldenTest2" $
    genAskBeaconName (AskAsset (adaSymbol,adaToken)) @?=
      expectedName "08bae3e35a3531a500149bd10d9b872621a41b4f6ba086920518220829370d2b"

-- | The offer beacon for a native asset.
goldenTest3 :: TestTree
goldenTest3 =
  testCase "goldenTest3" $
    genOfferBeaconName (OfferAsset (goldenPolicy,goldenName)) @?=
      expectedName "2dab55ec954afe698b6de75d81f0067fd503d1e4ecd49d593f8a3b9c9a963d4f"

-- | The ask beacon for a native asset.
goldenTest4 :: TestTree
goldenTest4 =
  testCase "goldenTest4" $
    genAskBeaconName (AskAsset (goldenPolicy,goldenName)) @?=
      expectedName "8530f818d752ab63a9366d8e5e38f1e83d45ae71240b08059fd8a3d4a2b6fea7"

-- | The offer beacon for a native asset with a maximum length (32 byte) asset name.
goldenTest5 :: TestTree
goldenTest5 =
  testCase "goldenTest5" $
    genOfferBeaconName (OfferAsset (goldenPolicy,goldenMaxName)) @?=
      expectedName "130f1a4999dcc8b6bb9f31949ee44381e587ace2f03437c65d15536770704324"

-- | The pair beacon for ADA -> native asset.
goldenTest6 :: TestTree
goldenTest6 =
  testCase "goldenTest6" $
    genPairBeaconName (OfferAsset (adaSymbol,adaToken)) (AskAsset (goldenPolicy,goldenName)) @?=
      expectedName "ac77181ee78354aa42afae2ad4b902e3c6f9137525d3c56fe20ea2ffc0a6a878"

-- | The pair beacon for native asset -> ADA.
goldenTest7 :: TestTree
goldenTest7 =
  testCase "goldenTest7" $
    genPairBeaconName (OfferAsset (goldenPolicy,goldenName)) (AskAsset (adaSymbol,adaToken)) @?=
      expectedName "ccf1b9d64f82b9d3b5f6bf4b0ac4ab0a55e489c20f7c078a4621c6ab2b83b717"

tests :: TestTree
tests = testGroup "Beacon Names"
  [ uniquenessTest1
  , uniquenessTest2
  , uniquenessTest3
  , uniquenessTest4
  , uniquenessTest5

  , goldenTest1
  , goldenTest2
  , goldenTest3
  , goldenTest4
  , goldenTest5
  , goldenTest6
  , goldenTest7
  ]
