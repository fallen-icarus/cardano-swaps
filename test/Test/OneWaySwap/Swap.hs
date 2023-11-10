{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.Swap
  (
  --   -- * Scenarios Tested
  --   -- ** Scenarios that should succeed
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                    <> singleton beaconSym askBeacon' 1
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',2),(offerBeacon',2),(askBeacon',2)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps!!1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',2),(offerBeacon',2),(askBeacon',2)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{prevInput = Just $ swaps2!!0}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
            <> singleton beaconCurrencySymbol pairBeacon' 1
            <> singleton beaconCurrencySymbol offerBeacon' 1
            <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
        , askBeacon = askBeacon2
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
        , askBeacon = askBeacon2
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
            <> singleton beaconCurrencySymbol pairBeacon' 1
            <> singleton beaconCurrencySymbol offerBeacon' 1
            <> singleton beaconCurrencySymbol askBeacon' 1
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps are for different pairs that are composed together. The
-- swaps are chained circularly.
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
      assetW = testToken3
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetX)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetZ) (AskAsset assetY)
      pairBeacon3 = genOneWayPairBeaconName (OfferAsset assetW) (AskAsset assetZ)
      pairBeacon4 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetW)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetY
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetZ
      offerBeacon3 = genOfferBeaconName $ OfferAsset assetW
      offerBeacon4 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetX
      askBeacon2 = genAskBeaconName $ AskAsset assetY
      askBeacon3 = genAskBeaconName $ AskAsset assetZ
      askBeacon4 = genAskBeaconName $ AskAsset assetW
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon1
        , askId = fst assetX
        , askName = snd assetX
        , askBeacon = askBeacon1
        , swapPrice = unsafeRatio 1_000_000 1 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetZ
        , offerName = snd assetZ
        , offerBeacon = offerBeacon2
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon2
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum3 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon3
        , offerId = fst assetW
        , offerName = snd assetW
        , offerBeacon = offerBeacon3
        , askId = fst assetZ
        , askName = snd assetZ
        , askBeacon = askBeacon3
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum4 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon4
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon4
        , askId = fst assetW
        , askName = snd assetW
        , askBeacon = askBeacon4
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1)
                  , (pairBeacon4,1),(offerBeacon4,1),(askBeacon4,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetW 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetZ 10
  swap3 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon3 1
                            <> singleton beaconCurrencySymbol offerBeacon3 1
                            <> singleton beaconCurrencySymbol askBeacon3 1
                            <> uncurry singleton assetW 10
  swap4 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon4 1
                            <> singleton beaconCurrencySymbol offerBeacon4 1
                            <> singleton beaconCurrencySymbol askBeacon4 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap2, swap3, swap4 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3{prevInput = Just swap3}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4{prevInput = Just swap4}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
                    <> uncurry singleton assetW 10
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
            <> singleton beaconCurrencySymbol pairBeacon' 1
            <> singleton beaconCurrencySymbol offerBeacon' 1
            <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
            <> singleton beaconCurrencySymbol pairBeacon' 1
            <> singleton beaconCurrencySymbol offerBeacon' 1
            <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Ask beacon withdrawn from swap address.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
            <> singleton beaconCurrencySymbol pairBeacon' 1
            <> singleton beaconCurrencySymbol offerBeacon' 1
            <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Not enough of the ask asset deposited.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                    , lovelaceValueOf 9_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong beaconId.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{beaconId = "", prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong pair beacon.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{pairBeacon = "", prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong offer id.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{offerId = fst ask, prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong offer name.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{offerName = snd ask, prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong offer beacon.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{offerBeacon = "", prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong ask id.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{askId = fst offer, prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong ask name.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{askName = snd offer, prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong ask beacon.
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{askBeacon = "", prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Output's SwapDatum has wrong swapPrice.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                           $ toDatum swapDatum{swapPrice = unsafeRatio 1 1, prevInput = Just swap}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym pairBeacon' 1
                    <> singleton beaconSym offerBeacon' 1
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for the same trading pair, the outputs are combined into a
-- single output.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',2),(offerBeacon',2),(askBeacon',2)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 2
                    <> singleton beaconCurrencySymbol offerBeacon' 2
                    <> singleton beaconCurrencySymbol askBeacon' 2
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the outputs are combined into a single
-- output.
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
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
        , askBeacon = askBeacon2
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetY 10
                    <> lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the pair beacons are mixed up in the
-- outputs.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
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
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
        , askBeacon = askBeacon2
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the offer beacons are mixed up in the
-- outputs.
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
        , askBeacon = askBeacon2
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetX 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing swaps for different trading pairs, the ask beacons are mixed up in the
-- outputs.
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
      assetW = testToken3
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetX)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetZ) (AskAsset assetY)
      pairBeacon3 = genOneWayPairBeaconName (OfferAsset assetW) (AskAsset assetZ)
      pairBeacon4 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetW)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetY
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetZ
      offerBeacon3 = genOfferBeaconName $ OfferAsset assetW
      offerBeacon4 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetX
      askBeacon2 = genAskBeaconName $ AskAsset assetY
      askBeacon3 = genAskBeaconName $ AskAsset assetZ
      askBeacon4 = genAskBeaconName $ AskAsset assetW
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon1
        , askId = fst assetX
        , askName = snd assetX
        , askBeacon = askBeacon1
        , swapPrice = unsafeRatio 1_000_000 1 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetZ
        , offerName = snd assetZ
        , offerBeacon = offerBeacon2
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon2
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum3 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon3
        , offerId = fst assetW
        , offerName = snd assetW
        , offerBeacon = offerBeacon3
        , askId = fst assetZ
        , askName = snd assetZ
        , askBeacon = askBeacon3
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum4 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon4
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon4
        , askId = fst assetW
        , askName = snd assetW
        , askBeacon = askBeacon4
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1)
                  , (pairBeacon4,1),(offerBeacon4,1),(askBeacon4,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetW 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetZ 10
  swap3 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon3 1
                            <> singleton beaconCurrencySymbol offerBeacon3 1
                            <> singleton beaconCurrencySymbol askBeacon3 1
                            <> uncurry singleton assetW 10
  swap4 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon4 1
                            <> singleton beaconCurrencySymbol offerBeacon4 1
                            <> singleton beaconCurrencySymbol askBeacon4 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap2, swap3, swap4 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3{prevInput = Just swap3}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4{prevInput = Just swap4}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
                    <> uncurry singleton assetW 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | An extraneous asset is stored in the swap.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                    <> singleton beaconSym askBeacon' 1
                    <> uncurry singleton testToken3 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum is not an inline datum.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',1),(offerBeacon',1),(askBeacon',1)]
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
                    <> singleton beaconSym askBeacon' 1
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
            <> singleton beaconSym askBeacon' 1
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
                    <> singleton beaconSym askBeacon' 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing mulitple swaps, the first swap output is invalid. This test and `failureTest23`
-- are to explicitly check that the order of swap outputs does not impact the transaction's
-- validity.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetX)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetZ) (AskAsset assetY)
      pairBeacon3 = genOneWayPairBeaconName (OfferAsset assetW) (AskAsset assetZ)
      pairBeacon4 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetW)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetY
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetZ
      offerBeacon3 = genOfferBeaconName $ OfferAsset assetW
      offerBeacon4 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetX
      askBeacon2 = genAskBeaconName $ AskAsset assetY
      askBeacon3 = genAskBeaconName $ AskAsset assetZ
      askBeacon4 = genAskBeaconName $ AskAsset assetW
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon1
        , askId = fst assetX
        , askName = snd assetX
        , askBeacon = askBeacon1
        , swapPrice = unsafeRatio 1_000_000 1 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetZ
        , offerName = snd assetZ
        , offerBeacon = offerBeacon2
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon2
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum3 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon3
        , offerId = fst assetW
        , offerName = snd assetW
        , offerBeacon = offerBeacon3
        , askId = fst assetZ
        , askName = snd assetZ
        , askBeacon = askBeacon3
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum4 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon4
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon4
        , askId = fst assetW
        , askName = snd assetW
        , askBeacon = askBeacon4
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1)
                  , (pairBeacon4,1),(offerBeacon4,1),(askBeacon4,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetW 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetZ 10
  swap3 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon3 1
                            <> singleton beaconCurrencySymbol offerBeacon3 1
                            <> singleton beaconCurrencySymbol askBeacon3 1
                            <> uncurry singleton assetW 10
  swap4 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon4 1
                            <> singleton beaconCurrencySymbol offerBeacon4 1
                            <> singleton beaconCurrencySymbol askBeacon4 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap2, swap3, swap4 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetY 9
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3{prevInput = Just swap3}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4{prevInput = Just swap4}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
                    <> uncurry singleton assetW 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing mulitple swaps, the first swap output is invalid. This test and `failureTest22`
-- are to explicitly check that the order of swap outputs does not impact the transaction's
-- validity.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
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
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetX)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetZ) (AskAsset assetY)
      pairBeacon3 = genOneWayPairBeaconName (OfferAsset assetW) (AskAsset assetZ)
      pairBeacon4 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetW)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetY
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetZ
      offerBeacon3 = genOfferBeaconName $ OfferAsset assetW
      offerBeacon4 = genOfferBeaconName $ OfferAsset assetY
      askBeacon1 = genAskBeaconName $ AskAsset assetX
      askBeacon2 = genAskBeaconName $ AskAsset assetY
      askBeacon3 = genAskBeaconName $ AskAsset assetZ
      askBeacon4 = genAskBeaconName $ AskAsset assetW
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon1
        , askId = fst assetX
        , askName = snd assetX
        , askBeacon = askBeacon1
        , swapPrice = unsafeRatio 1_000_000 1 
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetZ
        , offerName = snd assetZ
        , offerBeacon = offerBeacon2
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon2
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum3 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon3
        , offerId = fst assetW
        , offerName = snd assetW
        , offerBeacon = offerBeacon3
        , askId = fst assetZ
        , askName = snd assetZ
        , askBeacon = askBeacon3
        , swapPrice = unsafeRatio 1 1
        , prevInput = Nothing
        }
      swapDatum4 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon4
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon4
        , askId = fst assetW
        , askName = snd assetW
        , askBeacon = askBeacon4
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
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1)
                  , (pairBeacon4,1),(offerBeacon4,1),(askBeacon4,1)
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
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetZ 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetW 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
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
                            <> singleton beaconCurrencySymbol askBeacon1 1
                            <> uncurry singleton assetY 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetZ 10
  swap3 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon3 1
                            <> singleton beaconCurrencySymbol offerBeacon3 1
                            <> singleton beaconCurrencySymbol askBeacon3 1
                            <> uncurry singleton assetW 10
  swap4 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon4 1
                            <> singleton beaconCurrencySymbol offerBeacon4 1
                            <> singleton beaconCurrencySymbol askBeacon4 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap2, swap3, swap4 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{prevInput = Just swap2}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetY 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum3{prevInput = Just swap3}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon3 1
                    <> singleton beaconCurrencySymbol offerBeacon3 1
                    <> singleton beaconCurrencySymbol askBeacon3 1
                    <> uncurry singleton assetZ 9
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum4{prevInput = Just swap4}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon4 1
                    <> singleton beaconCurrencySymbol offerBeacon4 1
                    <> singleton beaconCurrencySymbol askBeacon4 1
                    <> uncurry singleton assetW 10
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
      pairBeacon' = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      offerBeacon' = genOfferBeaconName $ OfferAsset offer
      askBeacon' = genAskBeaconName $ AskAsset ask
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon'
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
              , mintTokens = [(pairBeacon',25),(offerBeacon',25),(askBeacon',25)]
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
                  <> singleton beaconCurrencySymbol askBeacon' 1
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
                        <> singleton beaconCurrencySymbol askBeacon' 1
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
      asks = 
        map (\i -> AskAsset (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      offers = 
        map (\i -> OfferAsset (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
      datums = 
        zipWith (\o@(OfferAsset offer) a@(AskAsset ask) -> 
              SwapDatum 
                { beaconId = beaconCurrencySymbol
                , pairBeacon = genOneWayPairBeaconName o a
                , offerId = fst offer
                , offerName = snd offer
                , offerBeacon = genOfferBeaconName o
                , askId = fst ask
                , askName = snd ask
                , askBeacon = genAskBeaconName a
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
            <> singleton beaconCurrencySymbol askBeacon 1
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
                  [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
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
                        <> singleton beaconCurrencySymbol askBeacon 1
                        <> singleton askId askName 10
                        ) 
                      )
                      swaps
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest1

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
        (assertEvaluationError "Wrong beacon_id") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Wrong pair_beacon") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Wrong offer_id") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Wrong offer_name") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Wrong offer_beacon") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Wrong ask_id") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Wrong ask_name") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Wrong ask_beacon") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Wrong swap_price") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Corresponding swap output not found") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Corresponding swap output not found") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Only the offered asset can leave the swap") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Only the offered asset can leave the swap") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "All swap datums must be inline datums") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "Fail: offer_taken * price <= ask_given") failureTest23

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 12
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 14

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 13
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 15
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest16
