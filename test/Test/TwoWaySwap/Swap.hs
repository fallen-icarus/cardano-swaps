{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TwoWaySwap.Swap
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
  , regressionTest10
  , regressionTest11
  , regressionTest12

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
-- | Swap with a single valid Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The pair is (native token,ADA). The swap is a forward swap.
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a single valid Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The pair is (native token,ADA). The swap is a reverse swap.
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
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
                    <> uncurry singleton assetY 10_000_000
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
            <> uncurry singleton assetY 10_000_000
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
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 20
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a multiple valid Swap UTxOs. Each swap is for the same trading pair and located at the
-- same address. Mints an unrelated token to an unrelated output in the same transaction to also 
-- check if the beacon policy can correctly ignore unrelated tokens and UTxOs. The pair is 
-- (native token,ADA). The swaps are forward swaps.
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!0}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a multiple valid Swap UTxOs. Each swap is for the same trading pair and located at the
-- same address. Mints an unrelated token to an unrelated output in the same transaction to also 
-- check if the beacon policy can correctly ignore unrelated tokens and UTxOs. The pair is 
-- (native token,ADA). The swaps are reverse swaps.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
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
                    <> uncurry singleton assetY 10_000_000
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!0}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 20
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 20
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a multiple valid Swap UTxOs. Each swap is for the same trading pair and located at
-- different addresses. Mints an unrelated token to an unrelated output in the same transaction to 
-- also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. The pair is 
-- (native token,ADA). The swaps are forward swaps.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
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
              { toAddress = swapAddr1
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
          , UtxoOutput
              { toAddress = swapAddr2
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

  swaps1 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swaps2 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr2


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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = swaps2
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps1!!0}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps2!!0}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a multiple valid Swap UTxOs. Each swap is for the same trading pair and located at
-- different addresses. Mints an unrelated token to an unrelated output in the same transaction to 
-- also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. The pair is 
-- (native token,ADA). The swaps are reverse swaps.
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
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
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10_000_000
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 10_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps1 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swaps2 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr2


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
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = swaps2
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps1!!0}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 20
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps2!!0}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetX 20
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When ADA is not part of the trading pair, ADA can still be deposited in case the minimum
-- required ADA amount for the UTxO increases.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 2 1
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
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 20
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps are for different pairs that are composed together. Each swap
-- is located at different addresses. Mints an unrelated token to an unrelated output in the same 
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. 
regressionTest8 :: EmulatorTrace ()
regressionTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps are for different pairs that are composed together. Each swap
-- is located at the same address. Mints an unrelated token to an unrelated output in the same 
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. 
regressionTest9 :: EmulatorTrace ()
regressionTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a swap in the same transaction where a another swap is made.
regressionTest10 :: EmulatorTrace ()
regressionTest10 = do
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
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',1),(asset1Beacon',1),(asset2Beacon',1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          , UtxoOutput
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

-- | Execute multiple swaps. The swaps are for different pairs that are composed together. Each swap
-- is located at the same address. The swaps are chained circularly. Mints an unrelated token to
-- an unrelated output in the same transaction to also check if the beacon policy can correctly 
-- ignore unrelated tokens and UTxOs. 
regressionTest11 :: EmulatorTrace ()
regressionTest11 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
      [asset11,asset21] = sort [assetX,assetY]
      [asset12,asset22] = sort [assetY,assetZ]
      [asset13,asset23] = sort [assetW,assetZ]
      [asset14,asset24] = sort [assetY,assetW]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
      pairBeacon1 = genTwoWayPairBeaconName assetX assetY
      pairBeacon2 = genTwoWayPairBeaconName assetY assetZ
      pairBeacon3 = genTwoWayPairBeaconName assetW assetZ
      pairBeacon4 = genTwoWayPairBeaconName assetY assetW
      asset1Beacon1 = genAssetBeaconName asset11
      asset1Beacon2 = genAssetBeaconName asset12
      asset1Beacon3 = genAssetBeaconName asset13
      asset1Beacon4 = genAssetBeaconName asset14
      asset2Beacon1 = genAssetBeaconName asset21
      asset2Beacon2 = genAssetBeaconName asset22
      asset2Beacon3 = genAssetBeaconName asset23
      asset2Beacon4 = genAssetBeaconName asset24
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , asset1Id = fst asset11
        , asset1Name = snd asset11
        , asset1Beacon = asset1Beacon1
        , asset2Id = fst asset21
        , asset2Name = snd asset21
        , asset2Beacon = asset2Beacon1
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , asset1Id = fst asset12
        , asset1Name = snd asset12
        , asset1Beacon = asset1Beacon2
        , asset2Id = fst asset22
        , asset2Name = snd asset22
        , asset2Beacon = asset2Beacon2
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum3 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon3
        , asset1Id = fst asset13
        , asset1Name = snd asset13
        , asset1Beacon = asset1Beacon3
        , asset2Id = fst asset23
        , asset2Name = snd asset23
        , asset2Beacon = asset2Beacon3
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum4 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon4
        , asset1Id = fst asset14
        , asset1Name = snd asset14
        , asset1Beacon = asset1Beacon4
        , asset2Id = fst asset24
        , asset2Name = snd asset24
        , asset2Beacon = asset2Beacon4
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1,1),(asset1Beacon1,1), (asset2Beacon1,1)
                  , (pairBeacon2,1),(asset1Beacon2,1), (asset2Beacon2,1)
                  , (pairBeacon3,1),(asset1Beacon3,1), (asset2Beacon3,1)
                  , (pairBeacon4,1),(asset1Beacon4,1), (asset2Beacon4,1)
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol asset1Beacon3 1
                    <> singleton beaconCurrencySymbol asset2Beacon3 1
                    <> uncurry singleton assetW 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol asset1Beacon4 1
                    <> singleton beaconCurrencySymbol asset2Beacon4 1
                    <> uncurry singleton assetY 10
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
  swap3 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon3 1
                            <> singleton beaconCurrencySymbol asset1Beacon3 1
                            <> singleton beaconCurrencySymbol asset2Beacon3 1
                            <> uncurry singleton assetW 10
  swap4 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon4 1
                            <> singleton beaconCurrencySymbol asset1Beacon4 1
                            <> singleton beaconCurrencySymbol asset2Beacon4 1
                            <> uncurry singleton assetY 10

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
              , spendRedeemer = 
                  toRedeemer $ getRequiredSwapDirection (OfferAsset assetZ) (AskAsset assetY)
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap2 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = 
                  toRedeemer $ getRequiredSwapDirection (OfferAsset assetW) (AskAsset assetZ)
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap3 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = 
                  toRedeemer $ getRequiredSwapDirection (OfferAsset assetY) (AskAsset assetW)
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap4 ]
              }
          -- , ScriptUtxoInput
          --     { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
          --     , spendRedeemer = 
          --         toRedeemer $ getRequiredSwapDirection (OfferAsset assetY) (AskAsset assetX)
          --     , spendFromAddress = swapAddr1
          --     , spendUtxos = [ swap1 ]
          --     }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3{prevInput = Just swap3}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol asset1Beacon3 1
                    <> singleton beaconCurrencySymbol asset2Beacon3 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4{prevInput = Just swap4}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol asset1Beacon4 1
                    <> singleton beaconCurrencySymbol asset2Beacon4 1
                    <> uncurry singleton assetW 10
                    )
                  -- , ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1}
                  --   , lovelaceValueOf 3_000_000 
                  --   <> singleton beaconCurrencySymbol pairBeacon1 1
                  --   <> singleton beaconCurrencySymbol asset1Beacon1 1
                  --   <> singleton beaconCurrencySymbol asset2Beacon1 1
                  --   <> uncurry singleton assetY 10
                  --   )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update a swap in the same transaction where another swap is made. The swaps are for the same
-- trading pair.
regressionTest12 :: EmulatorTrace ()
regressionTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 . unPaymentPubKeyHash 
                 . mockWalletPaymentPubKeyHash 
                 . knownWallet
      assetX = testToken1
      assetY = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash $ sellerCred 1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash $ sellerCred 2)
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
        , reversePrice = unsafeRatio 2 1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',3),(asset1Beacon',3),(asset2Beacon',3)]
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
              { toAddress = swapAddr1
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
          , UtxoOutput
              { toAddress = swapAddr2
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

  swap1 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swap2 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr2

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [(pairBeacon',-1),(asset1Beacon',-1),(asset2Beacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer 
                              $ getRequiredSwapDirection (OfferAsset assetX) (AskAsset assetY)
              , spendFromAddress = swapAddr1
              , spendUtxos = swap1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr2
              , spendUtxos = swap2
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ head swap1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton assetY 20
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 2 1}
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
-- | Swap input does not have a pair beacon.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = []
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Pair beacon withdrawn from swap address.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Asset1Beacon withdrawn from swap address.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Asset2Beacon withdrawn from swap address.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When using `ForwardSwap`, not enough of the ask asset given.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 6_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When using `ReverseSwap`, not enough of the ask asset given.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
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
                    <> uncurry singleton assetY 10_000_000
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
            <> uncurry singleton assetY 10_000_000
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
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
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

-- | Output's SwapDatum has wrong beaconId.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , beaconId = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong pairBeacon.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , pairBeacon = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong asset1Id.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , asset1Id = fst testToken3
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong asset1Name.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , asset1Name = snd testToken3
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong asset1Beacon.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , asset1Beacon = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong asset2Id.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , asset2Id = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong asset2Name.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , asset2Name = snd testToken3
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong asset2Beacon.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , asset2Beacon = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong forwardPrice.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , forwardPrice = unsafeRatio 0 1
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong reversePrice.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{ prevInput = Just swap
                                              , reversePrice = unsafeRatio 0 1
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for the same trading pair, the outputs are combined into a
-- single output.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!0}
                    , lovelaceValueOf 26_000_000 
                    <> singleton beaconSym pairBeacon' 2
                    <> singleton beaconSym asset1Beacon' 2
                    <> singleton beaconSym asset2Beacon' 2
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for different trading pairs from the same address, the outputs 
-- are combined into a single output.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 16_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for different trading pairs from the same address, the pair
-- beacons are swapped in the outputs.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for different trading pairs from the same address, the
-- asset1Beacons are swapped in the outputs.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for different trading pairs from the same address, the
-- asset2Beacons are swapped in the outputs.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | An extraneous asset is stored in the swap.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    <> uncurry singleton testToken5 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum is not an inline datum.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumHash $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym asset1Beacon' 1
                    <> singleton beaconSym asset2Beacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps, the first swap output is invalid. This test and `failureTest25`
-- are to explicitly check that the order of the outputs does not impact the transaction's
-- validity.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 12_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps, the first swap output is invalid. This test and `failureTest24`
-- are to explicitly check that the order of the outputs does not impact the transaction's
-- validity.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{prevInput = Just swap1 }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol asset1Beacon1 1
                    <> singleton beaconCurrencySymbol asset2Beacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol asset1Beacon2 1
                    <> singleton beaconCurrencySymbol asset2Beacon2 1
                    <> uncurry singleton assetY 9
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Swap multiple Swap UTxOs for the same trading pair and from the same address.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberSwapped = do
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  swaps <- take numberSwapped <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ForwardSwap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  map (\(ref,_) ->
                        ( Just $ TxOutDatumInline 
                               $ toDatum swapDatum{prevInput = Just ref}
                        , lovelaceValueOf 13_000_000 
                        <> singleton beaconCurrencySymbol pairBeacon' 1
                        <> singleton beaconCurrencySymbol asset1Beacon' 1
                        <> singleton beaconCurrencySymbol asset2Beacon' 1
                        ) 
                      )
                      swaps
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap multiple Swap UTxOs for different trading pairs and from the same address.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberSwapped = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      assetYs = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
      pairs = map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
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
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)

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

  void $ waitNSlots 2

  swaps <- take numberSwapped <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ReverseSwap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  map (\(ref,Just d@SwapDatum{..}) ->
                        ( Just $ TxOutDatumInline 
                               $ toDatum d{prevInput = Just ref}
                        , lovelaceValueOf 3_000_000 
                        <> singleton beaconCurrencySymbol pairBeacon 1
                        <> singleton beaconCurrencySymbol asset1Beacon 1
                        <> singleton beaconCurrencySymbol asset2Beacon 1
                        <> singleton asset2Id asset2Name 10
                        ) 
                      )
                      swaps
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest2

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `Swap` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Swap(s)"
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
    , checkPredicateOptions opts "regressionTest10"
        assertNoFailedTransactions regressionTest10
    , checkPredicateOptions opts "regressionTest11"
        assertNoFailedTransactions regressionTest11
    , checkPredicateOptions opts "regressionTest12"
        assertNoFailedTransactions regressionTest12

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Swap input missing pair beacon") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Corresponding swap output not found") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Only the offered asset can leave the swap") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Only the offered asset can leave the swap") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Wrong beacon_id") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Wrong pair_beacon") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Wrong asset1_id") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Wrong asset1_name") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Wrong asset1_beacon") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Wrong asset2_id") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Wrong asset2_name") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Wrong asset2_beacon") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Wrong forward_price") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Wrong reverse_price") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Corresponding swap output not found") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Only the asked asset or ada can be deposited into the swap") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Corresponding swap output not found") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Only the asked asset or ada can be deposited into the swap") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Only the asked asset or ada can be deposited into the swap") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Only the asked asset or ada can be deposited into the swap") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "All swap datums must be inline datums") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest25

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 11
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 14

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 12
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 15
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig regressionTest12
