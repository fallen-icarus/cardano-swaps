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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,-1)]
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

-- | Update a single Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The forward price was changed.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple swaps. Each swap is for the same trading pair. Mints an unrelated token to an 
-- unrelated output in the same transaction to also check if the beacon policy can correctly ignore 
-- unrelated tokens and UTxOs.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,2)]
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
              , outputUtxos = replicate 2 $
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,-2)]
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
              , outputUtxos = [ ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple swaps. Each swap is for the same trading pair. Mints an unrelated token to an 
-- unrelated output in the same transaction to also check if the beacon policy can correctly ignore 
-- unrelated tokens and UTxOs. One swap changes the forward price while the other changes the
-- reverse price.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,2)]
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
              , outputUtxos = replicate 2 $
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos =
                  [
                    ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton assetX 10
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{reversePrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple swaps. Each swap is for a different trading pair. Mints an unrelated token to 
-- an unrelated output in the same transaction to also check if the beacon policy can correctly 
-- ignore unrelated tokens and UTxOs. 
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
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetY,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      assetBeacon1 = genBeaconName (assetX,assetY)
      assetBeacon2 = genBeaconName (assetY,assetZ)
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY),(assetY,assetZ)]
              , mintTokens = [(assetBeacon1,1),(assetBeacon2,1)]
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
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol assetBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol assetBeacon2 1
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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon1,-1),(assetBeacon2,-1)]
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

-- | Update multiple swaps. Each swap is for a different trading pair. Mints an unrelated token to 
-- an unrelated output in the same transaction to also check if the beacon policy can correctly 
-- ignore unrelated tokens and UTxOs. 
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
                 $ knownWallet 3
      assetX = (adaSymbol,adaToken)
      assetY = testToken1
      assetZ = testToken2
      [asset1,asset2] = sort [assetX,assetY]
      [asset1',asset2'] = sort [assetY,assetZ]
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      assetBeacon1 = genBeaconName (assetX,assetY)
      assetBeacon2 = genBeaconName (assetY,assetZ)
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY),(assetY,assetZ)]
              , mintTokens = [(assetBeacon1,1),(assetBeacon2,1)]
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
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol assetBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol assetBeacon2 1
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
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum1{forwardPrice = unsafeRatio 1 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{reversePrice = unsafeRatio 1 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `CreateSwap` and `CloseOrUpdate` to change what trading pair a swap is for. Mints an 
-- unrelated token to an unrelated output in the same transaction to also check if the beacon 
-- policy can correctly ignore unrelated tokens and UTxOs.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let assetZ = testToken2
      [asset1',asset2'] = sort [assetY,assetZ]
      assetBeacon' = genBeaconName (assetY,assetZ)
      swapDatum' = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [(asset1,asset2),(asset1',asset2')]
              , mintTokens = [(assetBeacon,-1),(assetBeacon',1)]
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
                           $ toDatum swapDatum'
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon' 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `CreateSwap` and `CloseOrUpdate` to change what trading pair a swap is for in the same
-- transaction where another swap is closed. Mints an unrelated token to an unrelated output in the
-- same transaction to also check if the beacon policy can correctly ignore unrelated tokens and 
-- UTxOs.
regressionTest8 :: EmulatorTrace ()
regressionTest8 = do
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,2)]
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
              , outputUtxos = replicate 2 $
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  let assetZ = testToken2
      [asset1',asset2'] = sort [assetY,assetZ]
      assetBeacon' = genBeaconName (assetY,assetZ)
      swapDatum' = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [(asset1,asset2),(asset1',asset2')]
              , mintTokens = [(assetBeacon,-2),(assetBeacon',1)]
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
                  [
                    ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum'
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | When closing a single swap UTxO, withdraw the beacon.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,0)]
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

-- | When updating a swap UTxO, the UTxO is stored at the wrong a non-dapp address.
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
      nonSwapAddr = Address (ScriptCredential alwaysSucceedValidatorHash) 
                            (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
              { toAddress = nonSwapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing multiple swaps, not all beacons are burned.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,2)]
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
              , outputUtxos = replicate 2 $
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,-1)]
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
              , outputUtxos = [ ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new forward price is negative.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{forwardPrice = unsafeRatio (-2_000_000 ) 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new forward price is zero.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new reverse price is negative.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{reversePrice = unsafeRatio (-2_000_000 ) 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
      
-- | When updating a swap, the new reverse price is zero.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new forward price has a zero denominator.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let newDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafeBeaconName = assetBeacon
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
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
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new forward price has a negative denominator.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let newDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafeBeaconName = assetBeacon
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
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
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new reverse price has a zero denominator.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let newDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafeBeaconName = assetBeacon
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
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
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a swap, the new reverse price has a negative denominator.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let newDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafeBeaconName = assetBeacon
        , unsafeAsset1Id = fst asset1
        , unsafeAsset1Name = snd asset1
        , unsafeAsset2Id = fst asset2
        , unsafeAsset2Name = snd asset2
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
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating multiple swap UTxOs for the same trading pair, group up the beacons into the
-- same output.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,2)]
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
              , outputUtxos = replicate 2 $
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton assetX 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos =
                  [
                    ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 2
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating multiple swap UTxOs for different trading pairs, mix up the beacons in the
-- outputs.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
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
      assetBeacon1 = genBeaconName (assetX,assetY)
      assetBeacon2 = genBeaconName (assetY,assetZ)
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon1
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon2
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY),(assetY,assetZ)]
              , mintTokens = [(assetBeacon1,1),(assetBeacon2,1)]
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
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> uncurry singleton assetZ 10
                    ) 
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap1 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol assetBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol assetBeacon2 1
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
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum1{forwardPrice = unsafeRatio 1 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{reversePrice = unsafeRatio 1 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> uncurry singleton assetY 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the beacon id is changed.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{beaconId = ""}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the beacon name is changed.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{beaconName = ""}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the asset1 id is changed.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{asset1Id = fst asset2}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the asset1 name is changed.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{asset1Name = snd asset2}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the asset2 id is changed.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{asset2Id = fst asset1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the asset2 name is changed.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{asset2Name = snd asset1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the prev input is not Nothing.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single UTxO, store an extraneous asset in the UTxO.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton testToken10 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When composing `CreateSwap` and `CloseOrUpdate`, the old pair is not present in the 
-- `CreateSwap` redeemer.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let assetZ = testToken2
      [asset1',asset2'] = sort [assetY,assetZ]
      assetBeacon' = genBeaconName (assetY,assetZ)
      swapDatum' = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon'
        , asset1Id = fst asset1'
        , asset1Name = snd asset1'
        , asset2Id = fst asset2'
        , asset2Name = snd asset2'
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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [(asset1',asset2')]
              , mintTokens = [(assetBeacon,-1),(assetBeacon',1)]
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
                           $ toDatum swapDatum'
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon' 1
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Address owner did not approve.
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
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , forwardPrice = unsafeRatio 1_000_000 1
        , reversePrice = unsafeRatio 1 1_000_000
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,1)]
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
                    <> singleton beaconSym assetBeacon 1
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
            <> singleton beaconSym assetBeacon 1
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
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{forwardPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
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

  let numberCreated = 45
      sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,numberCreated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate (fromIntegral numberCreated)
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,numberCreated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate (fromIntegral numberCreated)
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
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
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,fromIntegral (-number))]
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

-- | Update multiple swaps for the same trading pair.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let numberCreated = 45
      sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      [asset1,asset2] = sort [assetX,assetY]
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      assetBeacon = genBeaconName (assetX,assetY)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = assetBeacon
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,numberCreated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate (fromIntegral numberCreated)
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
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
              , mintRedeemer = toRedeemer $ CreateSwap [(assetX,assetY)]
              , mintTokens = [(assetBeacon,numberCreated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate (fromIntegral numberCreated)
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
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
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{forwardPrice = unsafeRatio 1 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton assetX 10
                  )
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for different trading pairs.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let numberCreated = 20
      sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs =
        map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..80]
      assetYs = repeat ("","")
      pairs = 
        map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      assetBeacons = map genBeaconName pairs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = "" -- This will be replaced.
        , asset1Id = "" -- This will be replaced.
        , asset1Name = "" -- This wil be replaced.
        , asset2Id = "" -- This will be replaced.
        , asset2Name = "" -- This will be replaced.
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  let sampleOutputs =
        zipWith 
          (\(asset1,asset2) assetBeacon ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ asset1Id = fst asset1
                                      , asset1Name = snd asset1
                                      , beaconName = assetBeacon
                                      , asset2Id = fst asset2
                                      , asset2Name = snd asset2
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> uncurry singleton asset1 10
            )
          )
          pairs 
          assetBeacons

  let sampleMints = 
        TokenMint 
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateSwap pairs
          , mintTokens = zip assetBeacons $ repeat 1
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap $ take numberCreated pairs
              , mintTokens = take numberCreated $ zip assetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take numberCreated sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap 
                                          $ take numberCreated
                                          $ drop numberCreated pairs
              , mintTokens = take numberCreated $ drop numberCreated $ zip assetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take numberCreated $ drop numberCreated sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap 
                                          $ take numberCreated
                                          $ drop (2 * numberCreated) pairs
              , mintTokens = 
                  take numberCreated $ drop (2 * numberCreated) $ zip assetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take numberCreated $ drop (2 * numberCreated) sampleOutputs
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
          , mintTokens = map (\(_,Just d) -> (beaconName d,-1)) swaps
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

-- | Update multiple Swap UTxOs for different trading pairs.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 number = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let numberCreated = 20
      sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetXs =
        map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..80]
      assetYs = repeat ("","")
      pairs = 
        map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip assetXs assetYs
      assetBeacons = map genBeaconName pairs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = "" -- This will be replaced.
        , asset1Id = "" -- This will be replaced.
        , asset1Name = "" -- This wil be replaced.
        , asset2Id = "" -- This will be replaced.
        , asset2Name = "" -- This will be replaced.
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  let sampleOutputs =
        zipWith 
          (\(asset1,asset2) assetBeacon ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ asset1Id = fst asset1
                                      , asset1Name = snd asset1
                                      , beaconName = assetBeacon
                                      , asset2Id = fst asset2
                                      , asset2Name = snd asset2
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> uncurry singleton asset1 10
            )
          )
          pairs 
          assetBeacons

  let sampleMints = 
        TokenMint 
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateSwap pairs
          , mintTokens = zip assetBeacons $ repeat 1
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap $ take numberCreated pairs
              , mintTokens = take numberCreated $ zip assetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take numberCreated sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap 
                                          $ take numberCreated
                                          $ drop numberCreated pairs
              , mintTokens = take numberCreated $ drop numberCreated $ zip assetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take numberCreated $ drop numberCreated sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap 
                                          $ take numberCreated
                                          $ drop (2 * numberCreated) pairs
              , mintTokens = 
                  take numberCreated $ drop (2 * numberCreated) $ zip assetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take numberCreated $ drop (2 * numberCreated) sampleOutputs
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
              , outputUtxos =
                  map (\(_,Just d) ->
                        ( Just $ TxOutDatumInline $ toDatum d{forwardPrice = unsafeRatio 10 1}
                        , lovelaceValueOf 3_000_000
                        <> singleton beaconCurrencySymbol (beaconName d) 1
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
      beforeAssetBeacons = map genBeaconName beforePairs
      afterAssetBeacons = map genBeaconName afterPairs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , beaconName = "" -- This will be replaced.
        , asset1Id = "" -- This will be replaced.
        , asset1Name = "" -- This wil be replaced.
        , asset2Id = "" -- This will be replaced.
        , asset2Name = "" -- This will be replaced.
        , forwardPrice = unsafeRatio 1 1
        , reversePrice = unsafeRatio 1 1
        , prevInput = Nothing
        }

  (mintRef,spendRef) <- initializeScripts

  let sampleOutputs =
        zipWith 
          (\(asset1,asset2) assetBeacon ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ asset1Id = fst asset1
                                      , asset1Name = snd asset1
                                      , beaconName = assetBeacon
                                      , asset2Id = fst asset2
                                      , asset2Name = snd asset2
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> uncurry singleton asset1 10
            )
          )
          beforePairs
          beforeAssetBeacons

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap $ take 20 beforePairs
              , mintTokens = take 20 $ zip beforeAssetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 20 sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap $ take 20 $ drop 20 beforePairs
              , mintTokens = take 20 $ drop 20 $ zip beforeAssetBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 20 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  let newSampleOutputs =
        zipWith 
          (\(asset1,asset2) assetBeacon ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ asset1Id = fst asset1
                                      , asset1Name = snd asset1
                                      , beaconName = assetBeacon
                                      , asset2Id = fst asset2
                                      , asset2Name = snd asset2
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> uncurry singleton asset1 10
            )
          )
          afterPairs
          afterAssetBeacons

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [  
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer
                             $ CreateSwap 
                             $ take number beforePairs <> take number afterPairs
              , mintTokens = 
                  take number (zip beforeAssetBeacons $ repeat (-1)) <>
                  take number (zip afterAssetBeacons $ repeat 1)
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
              , outputUtxos = take number newSampleOutputs
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

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Swap forward_price not > 0") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Swap forward_price not > 0") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Swap reverse_price not > 0") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Swap reverse_price not > 0") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Swap forward_price denominator not > 0") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Swap forward_price denominator not > 0") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Swap reverse_price denominator not > 0") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Swap reverse_price denominator not > 0") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Beacons must be stored individually") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Invalid SwapDatum beacon_id") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Invalid SwapDatum asset1_id") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Invalid SwapDatum asset1_name") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Invalid SwapDatum asset2_id") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Invalid SwapDatum asset2_name") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "prev_input not None") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "No extraneous assets can be stored in the swap UTxO") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Only the beacons in the redeemer can be minted/burned") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "Staking credential did not approve") failureTest23

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 53
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 7
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 53
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 15
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 11

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 54
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 8
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 54
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 16
    , checkPredicateOptions opts "perfIncreaseTest5"
        (Test.not assertNoFailedTransactions) $ benchTest5 12
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest23
