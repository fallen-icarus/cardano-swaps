{-# LANGUAGE OverloadedStrings #-}

module Test.TwoWaySwap.BeaconNames
  (
    uniquenessTest1
  , uniquenessTest2

  , goldenTest1
  , goldenTest2
  , goldenTest3
  , goldenTest4
  , goldenTest5

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

-------------------------------------------------
-- Golden Tests
-------------------------------------------------
-- The expected hashes below are pinned by the golden tests in
-- aiken/lib/cardano_swaps/two_way_swap/utils.ak. Both test suites use the same inputs, so these
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

-- | The asset beacon for ADA.
goldenTest1 :: TestTree
goldenTest1 =
  testCase "goldenTest1" $
    genAssetBeaconName (adaSymbol,adaToken) @?=
      expectedName "07d5f63e85046b83e1fc4102a7c19c3f1711c51984725e3b6cf195900947cebe"

-- | The asset beacon for a native asset.
goldenTest2 :: TestTree
goldenTest2 =
  testCase "goldenTest2" $
    genAssetBeaconName (goldenPolicy,goldenName) @?=
      expectedName "2dab55ec954afe698b6de75d81f0067fd503d1e4ecd49d593f8a3b9c9a963d4f"

-- | The asset beacon for a native asset with a maximum length (32 byte) asset name.
goldenTest3 :: TestTree
goldenTest3 =
  testCase "goldenTest3" $
    genAssetBeaconName (goldenPolicy,goldenMaxName) @?=
      expectedName "130f1a4999dcc8b6bb9f31949ee44381e587ace2f03437c65d15536770704324"

-- | The pair beacon for the ADA/native asset pair, given in sorted order (ADA sorts first).
goldenTest4 :: TestTree
goldenTest4 =
  testCase "goldenTest4" $
    genPairBeaconName (adaSymbol,adaToken) (goldenPolicy,goldenName) @?=
      expectedName "c6656f7142e3ef8e72066251cac9e9cc2454571204fa85ab13b34a03e77d7230"

-- | The pair beacon for the same pair given in reverse order: the off-chain sorting must agree
-- with the on-chain lexicographical ordering, so the name must be identical to goldenTest4.
goldenTest5 :: TestTree
goldenTest5 =
  testCase "goldenTest5" $
    genPairBeaconName (goldenPolicy,goldenName) (adaSymbol,adaToken) @?=
      expectedName "c6656f7142e3ef8e72066251cac9e9cc2454571204fa85ab13b34a03e77d7230"

tests :: TestTree
tests = testGroup "Beacon Names"
  [ uniquenessTest1
  , uniquenessTest2

  , goldenTest1
  , goldenTest2
  , goldenTest3
  , goldenTest4
  , goldenTest5
  ]
