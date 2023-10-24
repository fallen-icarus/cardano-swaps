{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.CreateSwap
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3

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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
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
        , swapPrice = unsafeRatio 1 1_000_000
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
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
      offer1 = testToken1
      ask = (adaSymbol,adaToken)
      offer2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1 = genUnsortedPairBeaconName offer1 ask
      pairBeacon2 = genUnsortedPairBeaconName offer2 ask
      offerBeacon1 = uncurry genOfferBeaconName offer1
      offerBeacon2 = uncurry genOfferBeaconName offer2
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst offer1
        , offerName = snd offer1
        , offerBeacon = offerBeacon1
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst offer2
        , offerName = snd offer2
        , offerBeacon = offerBeacon2
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym offerBeacon1 1
                    <> uncurry singleton offer1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2 1
                    <> singleton beaconSym offerBeacon2 1
                    <> uncurry singleton offer2 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The trading pair corresponds to a different becon than the one actually minted. The datum
-- has the proper pair beacon name.
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [("other",1),(offerBeacon',1)]
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
                    <> singleton beaconCurrencySymbol "other" 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The trading pair corresponds to a different becon than the one actually minted. The datum
-- also has the wrong pair beacon name.
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
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = "other"
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [("other",1),(offerBeacon',1)]
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
                    <> singleton beaconCurrencySymbol "other" 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',2),(offerBeacon',1)]
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
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional pair beacon and offer beacon and withdraw them.
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional offer beacon and withdraw it.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',1),(offerBeacon',2)]
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
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an extra, unrelated token.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [("other",1),(pairBeacon',1),(offerBeacon',1)]
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
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional pair beacon and store it in the Swap UTxO.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',2),(offerBeacon',1)]
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
                    <> singleton beaconCurrencySymbol pairBeacon' 2
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an additional offer beacon and store it in the Swap UTxO.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = [(pairBeacon',1),(offerBeacon',2)]
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
                    <> singleton beaconCurrencySymbol offerBeacon' 2
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO is stored at a dApp address without staking.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) Nothing
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO is stored at a non-dApp address.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential alwaysSucceedValidatorHash) (Just $ StakingHash sellerCred)
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong beaconId.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        { beaconId = ""
        , pairBeacon = pairBeacon'
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong pair beacon.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , pairBeacon = ""
        , offerId = fst offer
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong offer Id.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , offerId = ""
        , offerName = snd offer
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong offer name.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , offerName = ""
        , offerBeacon = offerBeacon'
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong offer beacon.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , offerBeacon = ""
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong ask id.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , askId = fst offer
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has the wrong ask name.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , askName = snd offer
        , swapPrice = unsafeRatio 1_000_000 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The offer asset and the ask asset are the same asset.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = testToken1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a zero swapPrice.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , swapPrice = unsafeRatio 0 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a negative swapPrice.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
        , swapPrice = unsafeRatio (-1) 1
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a zero denominator for swap price.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeOfferId = fst offer
        , unsafeOfferName = snd offer
        , unsafeOfferBeacon = offerBeacon'
        , unsafeAskId = fst ask
        , unsafeAskName = snd ask
        , unsafeSwapPrice = (1,0)
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum has a negative denominator for swap price.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer = testToken1
      ask = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      pairBeacon' = genUnsortedPairBeaconName offer ask
      offerBeacon' = uncurry genOfferBeaconName offer
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon'
        , unsafeOfferId = fst offer
        , unsafeOfferName = snd offer
        , unsafeOfferBeacon = offerBeacon'
        , unsafeAskId = fst ask
        , unsafeAskName = snd ask
        , unsafeSwapPrice = (1,-1)
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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap UTxO has extraneous assets.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    <> uncurry singleton testToken3 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | SwapDatum is not an inline datum.
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
                  [ ( Just $ TxOutDatumHash $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol pairBeacon' 1
                    <> singleton beaconCurrencySymbol offerBeacon' 1
                    <> uncurry singleton offer 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs for different trading pairs, the pair beacons are mixed up
-- in the outputs.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer1 = testToken1
      ask = (adaSymbol,adaToken)
      offer2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1 = genUnsortedPairBeaconName offer1 ask
      pairBeacon2 = genUnsortedPairBeaconName offer2 ask
      offerBeacon1 = uncurry genOfferBeaconName offer1
      offerBeacon2 = uncurry genOfferBeaconName offer2
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst offer1
        , offerName = snd offer1
        , offerBeacon = offerBeacon1
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst offer2
        , offerName = snd offer2
        , offerBeacon = offerBeacon2
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconSym pairBeacon2 1
                    <> singleton beaconSym offerBeacon1 1
                    <> uncurry singleton offer1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym offerBeacon2 1
                    <> uncurry singleton offer2 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs for different trading pairs, the offer beacons are mixed up
-- in the outputs.
failureTest26 :: EmulatorTrace ()
failureTest26 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer1 = testToken1
      ask = (adaSymbol,adaToken)
      offer2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1 = genUnsortedPairBeaconName offer1 ask
      pairBeacon2 = genUnsortedPairBeaconName offer2 ask
      offerBeacon1 = uncurry genOfferBeaconName offer1
      offerBeacon2 = uncurry genOfferBeaconName offer2
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst offer1
        , offerName = snd offer1
        , offerBeacon = offerBeacon1
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst offer2
        , offerName = snd offer2
        , offerBeacon = offerBeacon2
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym offerBeacon2 1
                    <> uncurry singleton offer1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2 1
                    <> singleton beaconSym offerBeacon1 1
                    <> uncurry singleton offer2 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs, the first output is invalid. This test and `failureTest28`
-- are to explicitly check that the order of outputs does not impact the transaction's validity.
failureTest27 :: EmulatorTrace ()
failureTest27 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer1 = testToken1
      ask = (adaSymbol,adaToken)
      offer2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1 = genUnsortedPairBeaconName offer1 ask
      pairBeacon2 = genUnsortedPairBeaconName offer2 ask
      offerBeacon1 = uncurry genOfferBeaconName offer1
      offerBeacon2 = uncurry genOfferBeaconName offer2
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst offer1
        , offerName = snd offer1
        , offerBeacon = offerBeacon1
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst offer2
        , offerName = snd offer2
        , offerBeacon = offerBeacon2
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconSym pairBeacon1 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2 1
                    <> singleton beaconSym offerBeacon2 1
                    <> uncurry singleton offer2 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs, the first output is invalid. This test and `failureTest29`
-- are to explicitly check that the order of outputs does not impact the transaction's validity.
failureTest28 :: EmulatorTrace ()
failureTest28 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offer1 = testToken1
      ask = (adaSymbol,adaToken)
      offer2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol
      pairBeacon1 = genUnsortedPairBeaconName offer1 ask
      pairBeacon2 = genUnsortedPairBeaconName offer2 ask
      offerBeacon1 = uncurry genOfferBeaconName offer1
      offerBeacon2 = uncurry genOfferBeaconName offer2
      swapDatum1 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon1
        , offerId = fst offer1
        , offerName = snd offer1
        , offerBeacon = offerBeacon1
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
        , prevInput = Nothing
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconCurrencySymbol
        , pairBeacon = pairBeacon2
        , offerId = fst offer2
        , offerName = snd offer2
        , offerBeacon = offerBeacon2
        , askId = fst ask
        , askName = snd ask
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1)
                  , (pairBeacon2,1),(offerBeacon2,1)
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
                    <> singleton beaconSym pairBeacon1 1
                    <> singleton beaconSym offerBeacon1 1
                    <> uncurry singleton offer1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym pairBeacon2 1
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
              , mintRedeemer = toRedeemer CreateSwap 
              , mintTokens = 
                  [ (pairBeacon',fromIntegral numberCreated)
                  , (offerBeacon',fromIntegral numberCreated)
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
                  <> singleton beaconSym offerBeacon' 1
                  <> uncurry singleton offer 10
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
      asks = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [41..80]
      offers = map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..40]
      pairs = map (\(x,y) -> if x > y then (y,x) else (x,y)) $ zip offers asks
      datums = take numberCreated $
        map (\(offer,ask) -> 
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
            pairs
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)

  mintRef <- initializeBeaconPolicy

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

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest2

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CreateSwap` scenarios.
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

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Wrong pair_beacon") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "One-way swaps must have exactly two kinds of beacons") failureTest3
      -- Uses app_name in the error message so result cannot be hardcoded.
    , checkPredicateOptions opts "failureTest4"
        (Test.not assertNoFailedTransactions) failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "One-way swaps must have exactly two kinds of beacons") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "One-way swaps must have exactly two kinds of beacons") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "UTxO does not have exactly 1 offer_beacon") failureTest8
      -- Uses app_name in the error message so result cannot be hardcoded.
    , checkPredicateOptions opts "failureTest9"
        (Test.not assertNoFailedTransactions) failureTest9
      -- Uses app_name in the error message so result cannot be hardcoded.
    , checkPredicateOptions opts "failureTest10"
        (Test.not assertNoFailedTransactions) failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Wrong beacon_id") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Wrong pair_beacon") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Wrong pair_beacon") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Wrong pair_beacon") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Wrong offer_beacon") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Wrong pair_beacon") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Wrong pair_beacon") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Offer asset cannot be same as ask asset") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "swap_price not > 0") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "swap_price not > 0") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "swap_price denominator not > 0") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "swap_price denominator not > 0") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "No extraneous assets can be stored in the swap UTxO") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "All swap datums must be inline datums") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "UTxO does not have exactly 1 pair_beacon") failureTest25
    , checkPredicateOptions opts "failureTest26"
        (assertEvaluationError "UTxO does not have exactly 1 offer_beacon") failureTest26
    , checkPredicateOptions opts "failureTest27"
        (assertEvaluationError "One-way swaps must have exactly two kinds of beacons") failureTest27
    , checkPredicateOptions opts "failureTest28"
        (assertEvaluationError "One-way swaps must have exactly two kinds of beacons") failureTest28

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 39
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 31

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 40
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 32
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest28
