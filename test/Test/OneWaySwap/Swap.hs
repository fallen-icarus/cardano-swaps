{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.Swap
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

import Test.Internal
import Test.Config
import Test.OneWaySwap.Utils
import CardanoSwaps.Utils
import CardanoSwaps.OneWaySwap

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
-- The pair is (native token,ADA). 
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a multiple valid Swap UTxOs. Each swap is for the same trading pair and located at the
-- same address. Mints an unrelated token to an unrelated output in the same transaction to also 
-- check if the beacon policy can correctly ignore unrelated tokens and UTxOs. The pair is 
-- (native token,ADA). 
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',2),(offerBeacon',2)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap with a multiple valid Swap UTxOs. Each swap is for the same trading pair and located at
-- different addresses. Mints an unrelated token to an unrelated output in the same transaction to 
-- also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. The pair is 
-- (native token,ADA). 
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
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
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',2),(offerBeacon',2)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps2!!0}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When ADA is not part of the trading pair, ADA can still be deposited in case the minimum
-- required ADA amount for the UTxO increases.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton ask 20
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps are for different pairs that are composed together. Each swap
-- is located at different addresses. Mints an unrelated token to an unrelated output in the same 
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. 
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps are for different pairs that are composed together. Each swap
-- is located at the same address. Mints an unrelated token to an unrelated output in the same 
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs. 
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a swap in the same transaction where a another swap is made.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Offer beacon withdrawn from swap address.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Not enough of the ask asset deposited.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just swap}
                    , lovelaceValueOf 12_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong beaconId.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong pair beacon.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong offer id.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                                              , offerId = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong offer name.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                                              , offerName = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong offer beacon.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                                              , offerBeacon = ""
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong ask id.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                                              , askId = fst offer
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong ask name.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                                              , askName = snd offer
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong swap price.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                                              , swapPrice = unsafeRatio 0 1
                                              }
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for the same trading pair, the outputs are combined into a
-- single output.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',2),(offerBeacon',2)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 2
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the outputs are combined into a single
-- output.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 6_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetY 10
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the pair beacons are mixed up in the
-- outputs.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the offer beacons are mixed up in the
-- outputs.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | An extraneous asset is stored in the swap.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton testToken3 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum is not an inline datum.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1)]
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
                    <> singleton beaconSym offerBeacon' 1
                    <> uncurry singleton offer 10
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
            <> singleton beaconSym offerBeacon' 1
            <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                    <> singleton beaconSym offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing mulitple swaps, the first swap output is invalid. This test and `failureTest20`
-- are to explicitly check that the order of swap outputs does not impact the transaction's
-- validity.
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10_000_000
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10_000_000
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetY 9
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing mulitple swaps, the first swap output is invalid. This test and `failureTest19`
-- are to explicitly check that the order of swap outputs does not impact the transaction's
-- validity.
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genUnsortedPairBeaconName assetX assetY
      pairBeacon2 = genUnsortedPairBeaconName assetY assetZ
      offerBeacon1 = uncurry genOfferBeaconName assetX
      offerBeacon2 = uncurry genOfferBeaconName assetY
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , swapPrice = unsafeRatio 1 1_000_000 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , swapPrice = unsafeRatio 1 1
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
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetX 10_000_000
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
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
                            <> singleton beaconCurrencySymbol offerBeacon1 1
                            <> uncurry singleton assetX 10_000_000
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
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
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> uncurry singleton assetZ 9
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
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = [(pairBeacon',25),(offerBeacon',25)]
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
                  <> singleton beaconCurrencySymbol offerBeacon' 1
                  <> uncurry singleton offer 10
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
              , spendRedeemer = toRedeemer Swap
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
                        <> singleton beaconCurrencySymbol offerBeacon' 1
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
      asks = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      offers = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
      datums = 
        zipWith (\offer ask -> 
              SwapDatum 
                { beaconId = beaconCurrencySymbol
                , pairBeacon = genUnsortedPairBeaconName offer ask
                , offerId = fst offer
                , offerName = snd offer
                , offerBeacon = uncurry genOfferBeaconName offer
                , askId = fst ask
                , askName = snd ask
                , swapPrice = unsafeRatio 1 1
                , prevInput = Nothing
                }
            ) 
            offers
            asks
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)

  (mintRef,spendRef) <- initializeScripts

  let sampleOutputs = 
        map 
          (\datum@SwapDatum{..} ->
            ( Just $ TxOutDatumInline $ toDatum datum
            , lovelaceValueOf 3_000_000
            <> singleton beaconCurrencySymbol pairBeacon 1
            <> singleton beaconCurrencySymbol offerBeacon 1
            <> singleton offerId offerName 10
            )
          )
          datums

  let sampleMints = 
        TokenMint 
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer CreateSwap 
          , mintTokens = 
              concatMap 
                (\ SwapDatum{..} -> 
                  [(pairBeacon,1),(offerBeacon,1)]
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
              , spendRedeemer = toRedeemer Swap
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
                        <> singleton beaconCurrencySymbol offerBeacon 1
                        <> singleton askId askName 10
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

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Swap input missing pair beacon") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Corresponding swap output not found") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Only the offered asset can leave the swap") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Wrong beacon_id") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Wrong pair_beacon") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Wrong offer_id") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Wrong offer_name") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Wrong offer_beacon") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Wrong ask_id") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Wrong ask_name") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Wrong swap_price") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Corresponding swap output not found") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Corresponding swap output not found") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "All swap datums must be inline datums") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest20

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 12
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 15

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 13
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 16
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest20
