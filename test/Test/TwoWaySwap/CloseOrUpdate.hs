{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TwoWaySwap.CloseOrUpdate
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5
  , regressionTest6
  , regressionTest7
  , regressionTest8
  , regressionTest9

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

    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4
  , benchTest5

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
initializeScripts :: EmulatorTrace (TxOutRef,TxOutRef)
initializeScripts = do
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

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = swapScript
      , createReferenceScriptAddress = refScriptAddress
      , createReferenceScriptUTxO = 
          ( lovelaceValueOf minUTxOSpendRef
          , TxOutDatumInline $ toDatum ()
          )
      }

  void $ waitNSlots 2

  liftM2 (,) (txOutRefWithValue $ lovelaceValueOf minUTxOMintRef)
             (txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef)

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Close a single Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(pairBeacon',-1),(asset1Beacon',-1),(asset2Beacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple valid Swap UTxOs. Each swap is for the same trading pair. Mints an unrelated 
-- token to an unrelated output in the same transaction to also check if the beacon policy can 
-- correctly ignore unrelated tokens and UTxOs. The pair is (native token,ADA). 
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
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

  void $ waitNSlots 2

  swaps <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(pairBeacon',-2),(asset1Beacon',-2),(asset2Beacon',-2)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple swaps. The swaps are for different pairs. Mints an unrelated token to an 
-- unrelated output in the same transaction to also check if the beacon policy can correctly 
-- ignore unrelated tokens and UTxOs. 
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetY,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      pairBeacon2 = genTwoWayPairBeaconName assetY assetZ
      asset1Beacon1 = genAssetBeaconName asset1
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon1 = genAssetBeaconName asset2
      asset2Beacon2 = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
  
  ( mintRef, spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(asset1Beacon1,1), (asset2Beacon1,1)
                  , (pairBeacon2,1),(asset1Beacon2,1), (asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon1 1
                            <> singleton beaconCurrencySymbol asset1Beacon1 1
                            <> singleton beaconCurrencySymbol asset2Beacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol asset1Beacon2 1
                            <> singleton beaconCurrencySymbol asset2Beacon2 1
                            <> uncurry singleton assetZ 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (pairBeacon1,-1),(asset1Beacon1,-1), (asset2Beacon1,-1)
                  , (pairBeacon2,-1),(asset1Beacon2,-1), (asset2Beacon2,-1)
                  ]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update a single Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
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

-- | Update multiple valid Swap UTxOs. Each swap is for the same trading pair. Mints an unrelated 
-- token to an unrelated output in the same transaction to also check if the beacon policy can 
-- correctly ignore unrelated tokens and UTxOs. The pair is (native token,ADA). 
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
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

  void $ waitNSlots 2

  swaps <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
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

-- | Update multiple swaps. The swaps are for different pairs. Mints an unrelated token to an 
-- unrelated output in the same transaction to also check if the beacon policy can correctly 
-- ignore unrelated tokens and UTxOs. 
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetY,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      pairBeacon2 = genTwoWayPairBeaconName assetY assetZ
      asset1Beacon1 = genAssetBeaconName asset1
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon1 = genAssetBeaconName asset2
      asset2Beacon2 = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
  
  ( mintRef, spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(asset1Beacon1,1), (asset2Beacon1,1)
                  , (pairBeacon2,1),(asset1Beacon2,1), (asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon1 1
                            <> singleton beaconCurrencySymbol asset1Beacon1 1
                            <> singleton beaconCurrencySymbol asset2Beacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol asset1Beacon2 1
                            <> singleton beaconCurrencySymbol asset2Beacon2 1
                            <> uncurry singleton assetZ 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `CreateSwap` and `CloseOrUpdate` to change what trading pair a swap is for.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
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
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      asset1Beacon1 = genAssetBeaconName asset1
      asset2Beacon1 = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon1,1),(asset1Beacon1,1),(asset2Beacon1,1)]
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
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym asset1Beacon1 1
                    <> singleton beaconSym asset2Beacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon1 1
            <> singleton beaconSym asset1Beacon1 1
            <> singleton beaconSym asset2Beacon1 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let assetZ = testToken2
      [asset1',asset2'] = sort [assetY,assetZ]
      pairBeacon2 = genTwoWayPairBeaconName assetY assetZ
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon2 = genAssetBeaconName asset2' 
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,-1),(asset1Beacon1,-1),(asset2Beacon1,-1)
                  , (pairBeacon2,1),(asset1Beacon2,1),(asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2 1
                    <> singleton beaconSym asset1Beacon2 1
                    <> singleton beaconSym asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `CreateSwap` and `CloseOrUpdate` to change what trading pair a swap is for. Another
-- swap is closed in the same transaction.
regressionTest8 :: EmulatorTrace ()
regressionTest8 = do
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
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      asset1Beacon1 = genAssetBeaconName asset1
      asset2Beacon1 = genAssetBeaconName asset2
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon1,2),(asset1Beacon1,2),(asset2Beacon1,2)]
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
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym asset1Beacon1 1
                    <> singleton beaconSym asset2Beacon1 1
                    <> uncurry singleton assetX 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym asset1Beacon1 1
                    <> singleton beaconSym asset2Beacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let assetZ = testToken2
      [asset1',asset2'] = sort [assetY,assetZ]
      pairBeacon2 = genTwoWayPairBeaconName assetY assetZ
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon2 = genAssetBeaconName asset2' 
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,-2),(asset1Beacon1,-2),(asset2Beacon1,-2)
                  , (pairBeacon2,1),(asset1Beacon2,1),(asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2 1
                    <> singleton beaconSym asset1Beacon2 1
                    <> singleton beaconSym asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, move it to another address.
regressionTest9 :: EmulatorTrace ()
regressionTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
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

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | When closing a single swap UTxO, withdraw the pair beacon.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',0),(asset1Beacon',-1),(asset2Beacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing a single swap UTxO, withdraw the asset1Beacon.
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
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',-1),(asset1Beacon',0),(asset2Beacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing a single swap UTxO, withdraw the asset2Beacon.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',-1),(asset1Beacon',-1),(asset2Beacon',0)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing a single swap UTxO, mint a non-beacon asset with the minting policy.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',-1),(asset1Beacon',-1),(asset2Beacon',-1),("other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the new forward price is negative.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{forwardPrice = unsafeRatio ( -3 ) 1}
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

-- | When updating a single swap, the new forward price is zero.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 0 1}
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

-- | When updating a single swap, the new reverse price is negative.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{reversePrice = unsafeRatio ( -3 ) 1}
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

-- | When updating a single swap, the new reverse price is zero.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{reversePrice = unsafeRatio 0 1}
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

-- | When updating a single swap, the new forward price has a zero denominator.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  
  let newDatum = UnsafeDatum
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum newDatum
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

-- | When updating a single swap, the new forward price has a negative denominator.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  
  let newDatum = UnsafeDatum
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum newDatum
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
      
-- | When updating a single swap, the new reverse price has a zero denominator.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  
  let newDatum = UnsafeDatum
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum newDatum
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

-- | When updating a single swap, the new reverse price has a negative denominator.
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  
  let newDatum = UnsafeDatum
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum newDatum
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
      
-- | When updating multiple swaps for the same trading pair, group the beacons into a single UTxO.
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
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
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

  void $ waitNSlots 2

  swaps <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 2
                    <> singleton beaconSym asset1Beacon' 2
                    <> singleton beaconSym asset2Beacon' 2
                    <> uncurry singleton assetX 10
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating multiple swaps for different trading pairs, mix up the pair beacons in the 
-- outputs.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetY,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      pairBeacon2 = genTwoWayPairBeaconName assetY assetZ
      asset1Beacon1 = genAssetBeaconName asset1
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon1 = genAssetBeaconName asset2
      asset2Beacon2 = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
  
  ( mintRef, spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(asset1Beacon1,1), (asset2Beacon1,1)
                  , (pairBeacon2,1),(asset1Beacon2,1), (asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon1 1
                            <> singleton beaconCurrencySymbol asset1Beacon1 1
                            <> singleton beaconCurrencySymbol asset2Beacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol asset1Beacon2 1
                            <> singleton beaconCurrencySymbol asset2Beacon2 1
                            <> uncurry singleton assetZ 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating multiple swaps for different trading pairs, mix up the asset1Beacons in the 
-- outputs.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      assetW = testToken3
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetW,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      pairBeacon2 = genTwoWayPairBeaconName assetW assetZ
      asset1Beacon1 = genAssetBeaconName asset1
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon1 = genAssetBeaconName asset2
      asset2Beacon2 = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
  
  ( mintRef, spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(asset1Beacon1,1), (asset2Beacon1,1)
                  , (pairBeacon2,1),(asset1Beacon2,1), (asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon1 1
                            <> singleton beaconCurrencySymbol asset1Beacon1 1
                            <> singleton beaconCurrencySymbol asset2Beacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol asset1Beacon2 1
                            <> singleton beaconCurrencySymbol asset2Beacon2 1
                            <> uncurry singleton assetZ 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating multiple swaps for different trading pairs, mix up the asset2Beacons in the 
-- outputs.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      assetW = testToken3
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetW,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      pairBeacon2 = genTwoWayPairBeaconName assetW assetZ
      asset1Beacon1 = genAssetBeaconName asset1
      asset1Beacon2 = genAssetBeaconName asset1'
      asset2Beacon1 = genAssetBeaconName asset2
      asset2Beacon2 = genAssetBeaconName asset2'
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
  
  ( mintRef, spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(asset1Beacon1,1), (asset2Beacon1,1)
                  , (pairBeacon2,1),(asset1Beacon2,1), (asset2Beacon2,1)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon1 1
                            <> singleton beaconCurrencySymbol asset1Beacon1 1
                            <> singleton beaconCurrencySymbol asset2Beacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol asset1Beacon2 1
                            <> singleton beaconCurrencySymbol asset2Beacon2 1
                            <> uncurry singleton assetZ 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating the prices of a single swap, the beaconId in the datum is changed.
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
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ forwardPrice = unsafeRatio 3 1
                                              , beaconId = ""
                                              }
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

-- | When updating the prices of a single swap, the pairBeacon in the datum is changed.
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
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ forwardPrice = unsafeRatio 3 1
                                              , pairBeacon = ""
                                              }
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

-- | When updating the prices of a single swap, the asset1Id in the datum is changed.
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
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ forwardPrice = unsafeRatio 3 1
                                              , asset1Id = fst testToken3
                                              }
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

-- | When updating the prices of a single swap, the asset1Name in the datum is changed.
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
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ forwardPrice = unsafeRatio 3 1
                                              , asset1Name = "other"
                                              }
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

-- | When updating the prices of a single swap, the asset2Id in the datum is changed.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ forwardPrice = unsafeRatio 3 1
                                              , asset2Id = ""
                                              }
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

-- | When updating the prices of a single swap, the asset2Name in the datum is changed.
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
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = asset1Beacon'
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = asset2Beacon'
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ forwardPrice = unsafeRatio 3 1
                                              , asset2Name = "other"
                                              }
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

-- | When updating a single swap, store an extraneous asset in the UTxO.
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
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 10
                    <> uncurry singleton testToken5 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the address' staking credential did not approve.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
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

-- | When updating a swap, move it to a non-DEX address.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential alwaysSucceedValidatorHash) (Just $ StakingHash newCred)
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
        , reversePrice = unsafeRatio 2 1_000_000
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym pairBeacon' 1
            <> singleton beaconSym asset1Beacon' 1
            <> singleton beaconSym asset2Beacon' 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
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

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple swaps for the same trading pair.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
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

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',25),(asset1Beacon',25),(asset2Beacon',25)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 25
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',25),(asset1Beacon',25),(asset2Beacon',25)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 25
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',25),(asset1Beacon',25),(asset2Beacon',25)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 25
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (pairBeacon',fromIntegral (-number))
                  , (asset1Beacon',fromIntegral (-number))
                  , (asset2Beacon',fromIntegral (-number))
                  ]
              } 
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for different trading pairs.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs =
        map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..80]
      assetYs = repeat ("","")
      pairs = 
        map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      datums = 
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

  (mintRef,spendRef) <- initializeScripts

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
        map (\SwapDatum{..} -> 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateSwap 
                , mintTokens = [(pairBeacon,1),(asset1Beacon,1),(asset2Beacon,1)]
                }
            )
            datums

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 $ drop 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ drop 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 $ drop 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ drop 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  let sampleBurns = 
        TokenMint 
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer BurnBeacons 
          , mintTokens = 
              concatMap 
                (\(_,Just d) -> 
                  [ (pairBeacon d,-1),(asset1Beacon d,-1),(asset2Beacon d,-1)]
                )
                swaps
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleBurns ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple swaps for the same trading pair.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
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

  (mintRef,spendRef) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',25),(asset1Beacon',25),(asset2Beacon',25)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 25
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',25),(asset1Beacon',25),(asset2Beacon',25)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 25
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',25),(asset1Beacon',25),(asset2Beacon',25)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 25
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate (fromIntegral number)
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 3 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol asset1Beacon' 1
                  <> singleton beaconCurrencySymbol asset2Beacon' 1
                  <> uncurry singleton assetX 10
                  )
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for different trading pairs.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs =
        map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..80]
      assetYs = repeat ("","")
      pairs = 
        map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      datums = 
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

  (mintRef,spendRef) <- initializeScripts

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
        map (\SwapDatum{..} -> 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateSwap 
                , mintTokens = [(pairBeacon,1),(asset1Beacon,1),(asset2Beacon,1)]
                }
            )
            datums

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 $ drop 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ drop 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 $ drop 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ drop 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = []
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos =
                  map (\(_,Just d) ->
                        ( Just $ TxOutDatumInline $ toDatum d{forwardPrice = unsafeRatio 10 1}
                        , lovelaceValueOf 3_000_000
                        <> singleton beaconCurrencySymbol (pairBeacon d) 1
                        <> singleton beaconCurrencySymbol (asset1Beacon d) 1
                        <> singleton beaconCurrencySymbol (asset2Beacon d) 1
                        <> singleton (asset1Id d) (asset1Name d) 10
                        )
                      )
                      swaps
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `CloseOrUpdate` with `CreateSwap` to change what trading pairs multiple Swap UTxOs 
-- are for. All swaps start and end for different trading pairs.
benchTest5 :: Int -> EmulatorTrace ()
benchTest5 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs =
        map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..80]
      assetYs = repeat ("","")
      beforePairs = 
        take 40 $ map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      afterPairs = 
        drop 40 $ map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beforeDatums = 
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
            beforePairs
      afterDatums = 
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
            afterPairs

  (mintRef,spendRef) <- initializeScripts

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
          beforeDatums

  let sampleMints = 
        map (\SwapDatum{..} -> 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateSwap 
                , mintTokens = [(pairBeacon,1),(asset1Beacon,1),(asset2Beacon,1)]
                }
            )
            beforeDatums

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 25 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  let sampleBurns = 
        zipWith 
            (\(_,Just beforeDatum) afterDatum ->
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateSwap 
                , mintTokens = 
                    [ (pairBeacon beforeDatum,-1)
                    , (asset1Beacon beforeDatum,-1)
                    , (asset2Beacon beforeDatum,-1)
                    , (pairBeacon afterDatum,1)
                    , (asset1Beacon afterDatum,1)
                    , (asset2Beacon afterDatum,1)
                    ]
                }
            )
            swaps
            afterDatums

  let newOutputs = 
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
          afterDatums

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = sampleBurns
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take number newOutputs
              }
          ]
          
      , validityRange = ValidityInterval Nothing Nothing
      }

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest5

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CloseOrUpdate` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "CloseOrUpdate Swap(s)"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2
    , checkPredicateOptions opts "regressionTest3"
        assertNoFailedTransactions regressionTest3
    , checkPredicateOptions opts "regressionTest4"
        assertNoFailedTransactions regressionTest4
    , checkPredicateOptions opts "regressionTest5"
        assertNoFailedTransactions regressionTest5
    , checkPredicateOptions opts "regressionTest6"
        assertNoFailedTransactions regressionTest6
    , checkPredicateOptions opts "regressionTest7"
        assertNoFailedTransactions regressionTest7
    , checkPredicateOptions opts "regressionTest8"
        assertNoFailedTransactions regressionTest8
    , checkPredicateOptions opts "regressionTest9"
        assertNoFailedTransactions regressionTest9

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Two-way swaps must have exactly three kinds of beacons") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "forward_price not > 0") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "forward_price not > 0") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "reverse_price not > 0") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "reverse_price not > 0") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "forward_price denominator not > 0") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "forward_price denominator not > 0") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "reverse_price denominator not > 0") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "reverse_price denominator not > 0") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "UTxO does not have exactly 1 asset1_beacon") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "UTxO does not have exactly 1 asset2_beacon") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Wrong beacon_id") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Wrong pair_beacon") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Wrong pair_beacon") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Wrong pair_beacon") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Wrong pair_beacon") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Wrong pair_beacon") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "No extraneous assets can be stored in the swap UTxO") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "Staking credential did not approve") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "Validator returned false") failureTest25

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 52
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 51
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 5
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 5
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 5

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 53
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 52
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 6
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 6
    , checkPredicateOptions opts "perfIncreaseTest5"
        (Test.not assertNoFailedTransactions) $ benchTest5 6
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest25
