{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TwoWaySwap.CreateSwap
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4

    -- ** Scenarios that should fail
  , failureTest1
  , failureTest2
  , failureTest3
  , failureTest4
  , failureTest5
  , failureTest6
  , failureTest7
  , failureTest8
  , failureTest9
  , failureTest10
  , failureTest11
  , failureTest12
  , failureTest13
  , failureTest14
  , failureTest15
  , failureTest16
  , failureTest17
  , failureTest18
  , failureTest19
  , failureTest20
  , failureTest21
  , failureTest22
  , failureTest23
  , failureTest24
  , failureTest25
  , failureTest26
  , failureTest27
  , failureTest28
  , failureTest29
  , failureTest30
  , failureTest31
  , failureTest32
  , failureTest33
  , failureTest34
  , failureTest35
  , failureTest36
  , failureTest37
    
    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
    
    -- * Full test function
  , tests
  ) where

import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.String (fromString)
import Data.List (sort)

import Test.Internal
import Test.Config
import Test.TwoWaySwap.Utils
import CardanoSwaps.Utils
import CardanoSwaps.TwoWaySwap

-------------------------------------------------
-- Initialize reference script.
-------------------------------------------------
initializeBeaconPolicy :: EmulatorTrace TxOutRef
initializeBeaconPolicy = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = beaconScript
      , createReferenceScriptAddress = refScriptAddress
      , createReferenceScriptUTxO = 
          ( lovelaceValueOf minUTxOMintRef
          , TxOutDatumInline $ toDatum ()
          )
      }

  void $ waitNSlots 2

  txOutRefWithValue $ lovelaceValueOf minUTxOMintRef

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Create a single valid Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The pair is (native token,ADA).
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple valid Swap UTxOs. All swaps are for the same trading pair.
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',2),(asset1Beacon',2),(asset2Beacon',2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple valid Swap UTxOs. All swaps are for unique trading pairs.
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = (adaSymbol,adaToken)
      assetZ = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetZ,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      pairBeacon2' = genTwoWayPairBeaconName assetZ assetY
      asset1'Beacon' = genAssetBeaconName asset1'
      asset2'Beacon' = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1'Beacon'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2'Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1',1),(asset1Beacon',1),(asset2Beacon',1)
                  , (pairBeacon2',1),(asset1'Beacon',1),(asset2'Beacon',1)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon1' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2' 1
                    <> singleton beaconSym asset1'Beacon' 1
                    <> singleton beaconSym asset2'Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a single valid Swap UTxO. The UTxO has both asset1 and asset2.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The trading pair corresponds to a different beacon than the one actually minted. The
-- datum has the proper pair beacon name.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [("other",1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym "other" 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The trading pair corresponds to a different beacon than the one actually minted. The
-- datum also has the wrong pair beacon name.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = "other"
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [("other",1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym "other" 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional pair beacon and withdraw it.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',2),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional pair beacon, asset1Beacon, and asset2Beacon, and withdraw them.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',2),(asset1Beacon',2),(asset2Beacon',2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional asset1Beacon and withdraw it.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',2),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional asset2Beacon and withdraw it.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an extra, unrelated token.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1),("other",1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional pair beacon and store it in the Swap UTxO.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',2),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 2
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional asset1Beacon and store it in the Swap UTxO.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',2),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 2
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional asset2Beacon and store it in the Swap UTxO.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 2
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO is stored at a DApp address without staking.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) Nothing
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO is stored at a non-DApp address.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong beaconId.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = ""
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong pairBeacon.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = ""
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong asset1Id.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset2
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong asset1Name.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset2
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong asset1Beacon.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = ""
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong asset2Id.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset1
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong asset2Name.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset1
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong asset2Beacon.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = ""
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Asset1 and Asset2 are the same asset.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = assetX
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Asset1 and Asset2 are out of order.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset2
        , asset1Name = snd asset2
        , asset1Beacon = asset2Beacon'
        , asset2Id = fst asset1
        , asset2Name = snd asset1
        , asset2Beacon = asset1Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a zero forwardPrice.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 0 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a negative forwardPrice.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio ( -1_000_000 ) 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a zero denominator for forwardPrice.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset1Beacon = asset1Beacon'
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
        , unsafeAsset2Beacon = asset2Beacon'
        , unsafeForwardPrice = (1,0)
        , unsafeReversePrice = (1,1)
        , unsafePrevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a negative denominator for forwardPrice.
failureTest26 :: EmulatorTrace ()
failureTest26 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset1Beacon = asset1Beacon'
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
        , unsafeAsset2Beacon = asset2Beacon'
        , unsafeForwardPrice = (1,-1)
        , unsafeReversePrice = (1,1)
        , unsafePrevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a zero reversePrice.
failureTest27 :: EmulatorTrace ()
failureTest27 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 0 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a negative reversePrice.
failureTest28 :: EmulatorTrace ()
failureTest28 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio ( -1 ) 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a zero denominator for reversePrice.
failureTest29 :: EmulatorTrace ()
failureTest29 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset1Beacon = asset1Beacon'
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
        , unsafeAsset2Beacon = asset2Beacon'
        , unsafeForwardPrice = (1,1)
        , unsafeReversePrice = (1,0)
        , unsafePrevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a negative denominator for reversePrice.
failureTest30 :: EmulatorTrace ()
failureTest30 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset1Beacon = asset1Beacon'
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
        , unsafeAsset2Beacon = asset2Beacon'
        , unsafeForwardPrice = (1,1)
        , unsafeReversePrice = (1,-1)
        , unsafePrevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO has extraneous assets.
failureTest31 :: EmulatorTrace ()
failureTest31 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    <> uncurry singleton testToken2 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum is not an inline datum.
failureTest32 :: EmulatorTrace ()
failureTest32 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumHash $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs for different trading pairs, the pair beacons are mixed up
-- in the outputs.
failureTest33 :: EmulatorTrace ()
failureTest33 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = (adaSymbol,adaToken)
      assetZ = testToken2
      assetW = testToken3
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetZ,assetW]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      pairBeacon2' = genTwoWayPairBeaconName assetZ assetY
      asset1'Beacon' = genAssetBeaconName asset1'
      asset2'Beacon' = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1'Beacon'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2'Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1',1),(asset1Beacon',1),(asset2Beacon',1)
                  , (pairBeacon2',1),(asset1'Beacon',1),(asset2'Beacon',1)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon1' 1
                    <> singleton beaconSym asset1'Beacon' 1
                    <> singleton beaconSym asset2'Beacon' 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs for different trading pairs, the asset1Beacons are mixed up
-- in the outputs.
failureTest34 :: EmulatorTrace ()
failureTest34 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = (adaSymbol,adaToken)
      assetZ = testToken2
      assetW = testToken3
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetZ,assetW]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      pairBeacon2' = genTwoWayPairBeaconName assetZ assetY
      asset1'Beacon' = genAssetBeaconName asset1'
      asset2'Beacon' = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1'Beacon'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2'Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1',1),(asset1Beacon',1),(asset2Beacon',1)
                  , (pairBeacon2',1),(asset1'Beacon',1),(asset2'Beacon',1)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon1' 1
                    <> singleton beaconSym asset1'Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2'Beacon' 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs for different trading pairs, the asset2Beacons are mixed up
-- in the outputs.
failureTest35 :: EmulatorTrace ()
failureTest35 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = (adaSymbol,adaToken)
      assetZ = testToken2
      assetW = testToken3
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetZ,assetW]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      pairBeacon2' = genTwoWayPairBeaconName assetZ assetY
      asset1'Beacon' = genAssetBeaconName asset1'
      asset2'Beacon' = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1'Beacon'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2'Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1',1),(asset1Beacon',1),(asset2Beacon',1)
                  , (pairBeacon2',1),(asset1'Beacon',1),(asset2'Beacon',1)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon1' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2'Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2' 1
                    <> singleton beaconSym asset1'Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs, the first output is invalid. This test and 
-- `failureTest37` are to explicitly check that the order of the outputs does not impact
-- the transaction's validity.
failureTest36 :: EmulatorTrace ()
failureTest36 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',2),(asset1Beacon',2),(asset2Beacon',2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 0 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs, the first output is invalid. This test and 
-- `failureTest36` are to explicitly check that the order of the outputs does not impact
-- the transaction's validity.
failureTest37 :: EmulatorTrace ()
failureTest37 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetY = testToken1
      assetX = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',2),(asset1Beacon',2),(asset2Beacon',2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 0 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple Swap UTxOs for the same trading pair. The trading pair is (native asset,ADA).
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genTwoWayPairBeaconName assetX assetY
      asset1Beacon' = genAssetBeaconName asset1
      asset2Beacon' = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon',fromIntegral numberCreated)
                  , (asset1Beacon',fromIntegral numberCreated)
                  , (asset2Beacon',fromIntegral numberCreated)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberCreated
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym pairBeacon' 1
                  <> singleton beaconSym asset1Beacon' 1
                  <> singleton beaconSym asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for different trading pairs.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [31..60]
      assetYs = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..30]
      pairs = map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      datums = take numberCreated $
        map (\(asset1,asset2) -> 
              SwapDatum 
                { beaconId = beaconCurrencySymbol
                , pairBeacon = genTwoWayPairBeaconName asset1 asset2
                , asset1Id = fst asset1
                , asset1Name = snd asset1
                , asset1Beacon = genAssetBeaconName asset1
                , asset2Id = fst asset2
                , asset2Name = snd asset2
                , asset2Beacon = genAssetBeaconName asset2
                , forwardPrice = unsafeRatio 1 1
                , reversePrice = unsafeRatio 1 1
                , prevInput = Nothing
                }
            ) 
            pairs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)

  mintRef <- initializeBeaconPolicy

  let sampleOutputs = 
        map 
          (\datum@SwapDatum{..} ->
            ( Just $ TxOutDatumInline $ toDatum datum
            , lovelaceValueOf 3_000_000
            <> singleton beaconCurrencySymbol pairBeacon 1
            <> singleton beaconCurrencySymbol asset1Beacon 1
            <> singleton beaconCurrencySymbol asset2Beacon 1
            <> singleton asset1Id asset1Name 10
            )
          )
          datums

  let sampleMints = 
        TokenMint 
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer CreateOrCloseSwaps 
          , mintTokens = 
              concatMap 
                (\ SwapDatum{..} -> 
                  [(pairBeacon,1),(asset1Beacon,1),(asset2Beacon,1)]
                )
                datums
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest2

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CreateOrCloseSwaps` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create Swap(s)"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2
    , checkPredicateOptions opts "regressionTest3"
        assertNoFailedTransactions regressionTest3
    , checkPredicateOptions opts "regressionTest4"
        assertNoFailedTransactions regressionTest4

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "UTxO has wrong beacons") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Wrong pair_beacon") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Validator returned false") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "UTxO has wrong beacons") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "UTxO has wrong beacons") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "UTxO has wrong beacons") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Validator returned false") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Validator returned false") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Wrong beacon_id") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "UTxO has wrong beacons") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Wrong pair_beacon") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Wrong pair_beacon") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "UTxO has wrong beacons") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "No extraneous assets allowed in the UTxO") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "No extraneous assets allowed in the UTxO") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "UTxO has wrong beacons") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Asset1 must be less than asset2") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "forward_price numerator not > 0") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "forward_price numerator not > 0") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "forward_price denominator not > 0") failureTest25
    , checkPredicateOptions opts "failureTest26"
        (assertEvaluationError "forward_price denominator not > 0") failureTest26
    , checkPredicateOptions opts "failureTest27"
        (assertEvaluationError "reverse_price numerator not > 0") failureTest27
    , checkPredicateOptions opts "failureTest28"
        (assertEvaluationError "reverse_price numerator not > 0") failureTest28
    , checkPredicateOptions opts "failureTest29"
        (assertEvaluationError "reverse_price denominator not > 0") failureTest29
    , checkPredicateOptions opts "failureTest30"
        (assertEvaluationError "reverse_price denominator not > 0") failureTest30
    , checkPredicateOptions opts "failureTest31"
        (assertEvaluationError "No extraneous assets allowed in the UTxO") failureTest31
    , checkPredicateOptions opts "failureTest32"
        (assertEvaluationError "All swap datums must be inline datums") failureTest32
    , checkPredicateOptions opts "failureTest33"
        (assertEvaluationError "UTxO has wrong beacons") failureTest33
    , checkPredicateOptions opts "failureTest34"
        (assertEvaluationError "UTxO has wrong beacons") failureTest34
    , checkPredicateOptions opts "failureTest35"
        (assertEvaluationError "UTxO has wrong beacons") failureTest35
    , checkPredicateOptions opts "failureTest36"
        (assertEvaluationError "forward_price numerator not > 0") failureTest36
    , checkPredicateOptions opts "failureTest37"
        (assertEvaluationError "forward_price numerator not > 0") failureTest37

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 32
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 25

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 33
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 26
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest37
