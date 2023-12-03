{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.CloseOrUpdate
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5
  , regressionTest6

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

import Test.Internal
import Test.Config
import Test.OneWaySwap.Utils
import CardanoSwaps.Utils
import CardanoSwaps.OneWaySwap

{- | 

Since the logic for updating and closing are identical except for HOW the beacon_script is
executed, only one needs to be tested for errors. Since the plutus-apps emulator does not support
executing staking scripts, the only option is to test the minting exectution. The only way to
mint/burn beacons is to execute the beacon_script as a minting policy.

-}

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-2),(offerBeacon',-2),(askBeacon',-2)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
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
      offer = (adaSymbol,adaToken)
      ask = testToken1
      assetZ = testToken2
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = swapAddr1
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset ask) (AskAsset assetZ)
      offerBeacon1 = genOfferBeaconName $ OfferAsset offer
      offerBeacon2 = genOfferBeaconName $ OfferAsset ask
      askBeacon1 = genAskBeaconName $ AskAsset ask
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon1
        , askId = fst ask
        , askName = snd ask
        , askBeacon = askBeacon1
        , swapPrice = unsafeRatio 1_000_000 1
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst ask
        , offerName = snd ask
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
                    <> uncurry singleton offer 10
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
                    <> uncurry singleton ask 10
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
                            <> uncurry singleton offer 10
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton ask 10

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = 
                  [ (pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1)
                  , (pairBeacon2,-1),(offerBeacon2,-1),(askBeacon2,-1)
                  ]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr1
              , spendUtxos = [ swap1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `CreateOrCloseSwaps` and `SpendWithMint` to change what trading pair a swap is for.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
            <> singleton beaconCurrencySymbol pairBeacon1 1
            <> singleton beaconCurrencySymbol offerBeacon1 1
            <> singleton beaconCurrencySymbol askBeacon1 1
            <> uncurry singleton assetX 10
            )
            swapDatum

  let assetZ = testToken2
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , askBeacon = askBeacon2
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
                  ]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
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

-- | Compose `CreateOrCloseSwaps` and `SpendWithMint` to change what trading pair a swap is for. Another
-- swap is closed in the same transaction.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      assetX = testToken1
      assetY = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon1 = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      offerBeacon1 = genOfferBeaconName $ OfferAsset assetX
      askBeacon1 = genAskBeaconName $ AskAsset assetY
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon1
        , askId = fst assetY
        , askName = snd assetY
        , askBeacon = askBeacon1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon1,2),(offerBeacon1,2),(askBeacon1,2)]
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
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> singleton beaconCurrencySymbol askBeacon1 1
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
      pairBeacon2 = genOneWayPairBeaconName (OfferAsset assetY) (AskAsset assetZ)
      offerBeacon2 = genOfferBeaconName $ OfferAsset assetY
      askBeacon2 = genAskBeaconName $ AskAsset assetZ
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst assetY
        , offerName = snd assetY
        , offerBeacon = offerBeacon2
        , askId = fst assetZ
        , askName = snd assetZ
        , askBeacon = askBeacon2
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1,-2),(offerBeacon1,-2),(askBeacon1,-2)
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1)
                  ]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
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

-- | Update a single Swap UTxO and move it to a different swap address.
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 3 1}
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',0),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing a single swap UTxO, withdraw the offer beacon.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',0),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing a single swap UTxO, withdraw the ask beacon.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',0)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1),("other",1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the new price is negative.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio (-3) 1}
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

-- | When updating a single swap, the new price is zero.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 0 1}
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

-- | When updating a single swap, the new price has a zero denominator.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  let newDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeOfferId = fst offer
        , unsafeOfferName = snd offer
        , unsafeOfferBeacon = offerBeacon'
        , unsafeAskId = fst ask
        , unsafeAskName = snd ask
        , unsafeAskBeacon = askBeacon'
        , unsafeSwapPrice = (1,0)
        , unsafePrevInput = Nothing
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newDatum
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

-- | When updating a single swap, the new price has a negative denominator.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  let newDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeOfferId = fst offer
        , unsafeOfferName = snd offer
        , unsafeOfferBeacon = offerBeacon'
        , unsafeAskId = fst ask
        , unsafeAskName = snd ask
        , unsafeAskBeacon = askBeacon'
        , unsafeSwapPrice = (1,-1)
        , unsafePrevInput = Nothing
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newDatum
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

-- | When updating mulitple swaps for the same trading pair, group the beacons into a single UTxO.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = [(pairBeacon',3),(offerBeacon',3),(askBeacon',3)]
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 6_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 2
                    <> singleton beaconCurrencySymbol offerBeacon' 2
                    <> singleton beaconCurrencySymbol askBeacon' 2
                    <> uncurry singleton offer 10
                    )
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating mulitple swaps for different trading pairs, mix up the pair beacons.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
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
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1,2),(offerBeacon1,2),(askBeacon1,2)
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum1
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

  swaps1 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr1
              , spendUtxos = swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetY 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating mulitple swaps for different trading pairs, mix up the offer beacons.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
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
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1,2),(offerBeacon1,2),(askBeacon1,2)
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum1
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

  swaps1 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr1
              , spendUtxos = swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetY 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating mulitple swaps for different trading pairs, mix up the ask beacons.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
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
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
              , mintTokens = 
                  [ (pairBeacon1,2),(offerBeacon1,2),(askBeacon1,2)
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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum1
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

  swaps1 <- map fst <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swap2 <- txOutRefWithValue $ lovelaceValueOf 3_000_000 
                            <> singleton beaconCurrencySymbol pairBeacon2 1
                            <> singleton beaconCurrencySymbol offerBeacon2 1
                            <> singleton beaconCurrencySymbol askBeacon2 1
                            <> uncurry singleton assetY 10

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr1
              , spendUtxos = swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr2
              , spendUtxos = [ swap2 ]
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon1 1
                    <> singleton beaconCurrencySymbol offerBeacon1 1
                    <> singleton beaconCurrencySymbol askBeacon2 1
                    <> uncurry singleton assetX 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon2 1
                    <> singleton beaconCurrencySymbol offerBeacon2 1
                    <> singleton beaconCurrencySymbol askBeacon1 1
                    <> uncurry singleton assetY 10
                    ) 
                  ]
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap, the beaconId is changed.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{beaconId = ""}
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

-- | When updating a single swap, the pairBeacon is changed.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{pairBeacon = ""}
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

-- | When updating a single swap, the offerId is changed.
failureTest15  :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{offerId = fst ask}
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

-- | When updating a single swap, the offerName is changed.
failureTest16  :: EmulatorTrace ()
failureTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{offerName = snd ask}
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

-- | When updating a single swap, the offerBeacon is changed.
failureTest17  :: EmulatorTrace ()
failureTest17 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{offerBeacon = ""}
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

-- | When updating a single swap, the askId is changed.
failureTest18  :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{askId = fst offer}
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

-- | When updating a single swap, the askName is changed.
failureTest19  :: EmulatorTrace ()
failureTest19 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{askName = snd offer}
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

-- | When updating a single swap, the askBeacon is changed.
failureTest20  :: EmulatorTrace ()
failureTest20 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{askBeacon = ""}
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

-- | When updating a single swap, store an extraneous asset in the swap.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 3 1}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> singleton beaconCurrencySymbol askBeacon' 1
                    <> uncurry singleton offer 10
                    <> uncurry singleton testToken7 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The address' staking credential did not approve.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 3 1}
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

-- | When updating a single swap, move the swap to a non-DEX address.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      newCred = PubKeyCredential
              $ unPaymentPubKeyHash
              $ mockWalletPaymentPubKeyHash
              $ knownWallet 2
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      newAddr = Address (ScriptCredential alwaysSucceedValidatorHash) (Just $ StakingHash newCred)
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress,mintRef)
                  )
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = [(pairBeacon',-1),(offerBeacon',-1),(askBeacon',-1)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = newAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 3 1}
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

-- | When closing swaps, the beacon script is not executed as a minting policy.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = [ ]
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
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon' = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      offerBeacon' = genOfferBeaconName $ OfferAsset assetX
      askBeacon' = genAskBeaconName $ AskAsset assetY
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon'
        , askId = fst assetY
        , askName = snd assetY
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = 
                  [ (pairBeacon',fromIntegral (-number))
                  , (offerBeacon',fromIntegral (-number))
                  , (askBeacon',fromIntegral (-number))
                  ]
              } 
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
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
        zipWith (\offer ask -> 
              SwapDatum 
                { beaconId = beaconCurrencySymbol
                , pairBeacon = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
                , offerId = fst offer
                , offerName = snd offer
                , offerBeacon = genOfferBeaconName $ OfferAsset offer
                , askId = fst ask
                , askName = snd ask
                , askBeacon = genAskBeaconName $ AskAsset ask
                , swapPrice = unsafeRatio 1 1
                , prevInput = Nothing
                }
            ) 
            assetYs
            assetXs

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
        map (\SwapDatum{..} -> 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateOrCloseSwaps 
                , mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
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
          , mintRedeemer = toRedeemer CreateOrCloseSwaps 
          , mintTokens = 
              concatMap 
                (\(_,Just d) -> 
                  [ (pairBeacon d,-1),(offerBeacon d,-1),(askBeacon d,-1)]
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
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple swaps for the same trading pair. In order to actually get the minting policy
-- to executed, one swap must be closed. This should be negligible, though.
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
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon' = genOneWayPairBeaconName (OfferAsset assetX) (AskAsset assetY)
      offerBeacon' = genOfferBeaconName $ OfferAsset assetX
      askBeacon' = genAskBeaconName $ AskAsset assetY
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon'
        , offerId = fst assetX
        , offerName = snd assetX
        , offerBeacon = offerBeacon'
        , askId = fst assetY
        , askName = snd assetY
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps 
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
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintTokens = 
                  [ (pairBeacon',-1)
                  , (offerBeacon',-1)
                  , (askBeacon',-1)
                  ]
              } 
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate (fromIntegral number - 1) -- one swap must be closed
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 3 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol pairBeacon' 1
                  <> singleton beaconCurrencySymbol offerBeacon' 1
                  <> singleton beaconCurrencySymbol askBeacon' 1
                  <> uncurry singleton assetX 10
                  )
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for different trading pairs. In order to actually get the minting 
-- policy to executed, one swap must be closed. This should be negligible, though.
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
        zipWith (\offer ask -> 
              SwapDatum 
                { beaconId = beaconCurrencySymbol
                , pairBeacon = genOneWayPairBeaconName (OfferAsset offer) (AskAsset ask)
                , offerId = fst offer
                , offerName = snd offer
                , offerBeacon = genOfferBeaconName $ OfferAsset offer
                , askId = fst ask
                , askName = snd ask
                , askBeacon = genAskBeaconName $ AskAsset ask
                , swapPrice = unsafeRatio 1 1
                , prevInput = Nothing
                }
            ) 
            assetYs
            assetXs

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
        map (\SwapDatum{..} -> 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateOrCloseSwaps 
                , mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
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
          , mintRedeemer = toRedeemer CreateOrCloseSwaps 
          , mintTokens = 
              concatMap 
                (\(_,Just d) -> 
                  [ (pairBeacon d,-1),(offerBeacon d,-1),(askBeacon d,-1)]
                )
                (take 1 swaps)
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleBurns ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer SpendWithMint
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos =
                  map (\(_,Just d) ->
                        ( Just $ TxOutDatumInline $ toDatum d{swapPrice = unsafeRatio 10 1}
                        , lovelaceValueOf 3_000_000
                        <> singleton beaconCurrencySymbol (pairBeacon d) 1
                        <> singleton beaconCurrencySymbol (offerBeacon d) 1
                        <> singleton beaconCurrencySymbol (askBeacon d) 1
                        <> singleton (offerId d) (offerName d) 10
                        )
                      )
                      (drop 1 swaps)
              }  
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose `SpendWithMint` with `CreateOrCloseSwaps` to change what trading pairs multiple Swap UTxOs 
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
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      datums = 
        zipWith (\asset1 asset2 -> 
              SwapDatum 
                { beaconId = beaconCurrencySymbol
                , pairBeacon = genOneWayPairBeaconName (OfferAsset asset1) (AskAsset asset2)
                , offerId = fst asset1
                , offerName = snd asset1
                , offerBeacon = genOfferBeaconName $ OfferAsset asset1
                , askId = fst asset2
                , askName = snd asset2
                , askBeacon = genAskBeaconName $ AskAsset asset2
                , swapPrice = unsafeRatio 1 1
                , prevInput = Nothing
                }
            ) 
            assetYs
            assetXs
      beforeDatums = take 40 datums
      afterDatums = drop 40 datums

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
          beforeDatums

  let sampleMints = 
        map (\SwapDatum{..} -> 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateOrCloseSwaps 
                , mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
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
        zipWith 
            (\(_,Just beforeDatum) afterDatum ->
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer CreateOrCloseSwaps 
                , mintTokens = 
                    [ (pairBeacon beforeDatum,-1)
                    , (offerBeacon beforeDatum,-1)
                    , (askBeacon beforeDatum,-1)
                    , (pairBeacon afterDatum,1)
                    , (offerBeacon afterDatum,1)
                    , (askBeacon afterDatum,1)
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
            <> singleton beaconCurrencySymbol offerBeacon 1
            <> singleton beaconCurrencySymbol askBeacon 1
            <> singleton offerId offerName 10
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
              , spendRedeemer = toRedeemer SpendWithMint
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

benchTrace :: (Int -> EmulatorTrace ()) -> Int -> IO ()
benchTrace f i = runEmulatorTraceIO' def emConfig $ f i

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all create/update scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create/Update Swap(s)"
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

       -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "One-way swaps must have exactly three kinds of beacons") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "One-way swaps must have exactly three kinds of beacons") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "One-way swaps must have exactly three kinds of beacons") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "One-way swaps must have exactly three kinds of beacons") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "swap_price numerator not > 0") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "swap_price numerator not > 0") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "swap_price denominator not > 0") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "swap_price denominator not > 0") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "UTxO does not have exactly 1 offer_beacon") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "UTxO does not have exactly 1 ask_beacon") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Wrong beacon_id") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Wrong pair_beacon") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Wrong pair_beacon") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Wrong pair_beacon") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Wrong offer_beacon") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Wrong pair_beacon") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Wrong pair_beacon") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Wrong ask_beacon") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "No extraneous assets can be stored in the swap UTxO") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Staking credential did not approve") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "Validator returned false") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "Beacon script not executed as minting policy") failureTest24

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 56
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 56
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 26
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 28
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 26

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 57
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 57
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 27
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 29
    , checkPredicateOptions opts "perfIncreaseTest5"
        (Test.not assertNoFailedTransactions) $ benchTest5 27

    ]

testTrace :: EmulatorTrace () -> IO ()
testTrace = runEmulatorTraceIO' def emConfig
