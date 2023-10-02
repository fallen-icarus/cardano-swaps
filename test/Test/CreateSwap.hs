{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.CreateSwap
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
  , failureTest21
  , failureTest22
  , failureTest23
  , failureTest24
  , failureTest25
  , failureTest26

    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4
  , benchTest5
  , benchTest6
  , benchTest7

    -- * Full test function
  , tests
  ) where

import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.String (fromString)
import Data.List (zip4)

import Test.Internal
import Test.Config
import CardanoSwaps

-------------------------------------------------
-- Initialize reference script.
-------------------------------------------------
initializeBeaconPolicies :: [(CurrencySymbol,TokenName)] -> EmulatorTrace [TxOutRef]
initializeBeaconPolicies assets = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let idxs = [ fromIntegral n + minUTxOMintRef | n <- [0..length assets] ]

  zipWithM_ 
    (\asset deposit -> do
      callEndpoint @"create-reference-script" h1 $
        CreateReferenceScriptParams
          { createReferenceScriptScript = beaconScript asset
          , createReferenceScriptAddress = refScriptAddress
          , createReferenceScriptUTxO = 
              ( lovelaceValueOf deposit
              , TxOutDatumInline $ toDatum ()
              )
          }

      void $ waitNSlots 2
    )
    assets
    idxs

  mapM (txOutRefWithValue . lovelaceValueOf) idxs

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Create a single valid Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The offered asset is a native token and the asked asset is ADA.
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a single valid Swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The offered asset is ADA and the asked asset is a native token.
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken1
      offerAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10_000_000
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a multiple valid Swap UTxOs. All swaps are for the same trading pair. The offered asset 
-- is a native token and the asked asset is ADA.
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,2)]
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
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a multiple valid Swap UTxOs. All swaps are for the same trading pair. The offered asset 
-- is ADA and the asked asset is a native token.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken1
      offerAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,2)]
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
                    <> uncurry singleton offerAsset 1_000_000
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 1_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a multiple valid Swap UTxOs for the same offer asset but different asked assets. The
-- offer asset is a native asset while one ask asset is a native asset and one ask asset is ADA.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset1 = (adaSymbol,adaToken)
      askAsset2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon1 = genBeaconName askAsset1
      assetBeacon2 = genBeaconName askAsset2
      swapDatum1 = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon1
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon2
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 10 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset1,askAsset2]
              , mintTokens = [(assetBeacon1,1),(assetBeacon2,1)]
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
                    <> singleton beaconSym assetBeacon1 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon2 1
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a multiple valid Swap UTxOs for different offer assets and different asked assets. The
-- offer assets are native assets while one ask asset is a native asset and one ask asset is ADA.
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      offerAsset2 = testToken3
      askAsset1 = (adaSymbol,adaToken)
      askAsset2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon1 = genBeaconName askAsset1
      assetBeacon2 = genBeaconName askAsset2
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 10 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset1,offerAsset2]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset1
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset1]
              , mintTokens = [(assetBeacon1,1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset2
                  , Just (refScriptAddress, mintRefs!!1)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset2]
              , mintTokens = [(assetBeacon2,1)]
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
                    <> singleton beaconSym1 assetBeacon1 1
                    <> uncurry singleton offerAsset1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym2 assetBeacon2 1
                    <> uncurry singleton offerAsset2 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create a multiple valid Swap UTxOs for different offer assets but the same asked asset. The
-- offer assets are native assets while the ask asset is ADA.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      offerAsset2 = testToken3
      askAsset1 = (adaSymbol,adaToken)
      askAsset2 = askAsset1
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon1 = genBeaconName askAsset1
      assetBeacon2 = genBeaconName askAsset2
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 10 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset1,offerAsset2]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset1
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset1]
              , mintTokens = [(assetBeacon1,1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset2
                  , Just (refScriptAddress, mintRefs!!1)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset2]
              , mintTokens = [(assetBeacon2,1)]
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
                    <> singleton beaconSym1 assetBeacon1 1
                    <> uncurry singleton offerAsset1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym2 assetBeacon2 1
                    <> uncurry singleton offerAsset2 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The redeemer asset list has a different asset than the one corresponding to the beacon minted.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [testToken5]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The redeemer asset list is empty.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap []
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an addition beacon for an asset that is not in the redeemer's list.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,1),(genBeaconName testToken5,1)]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Withdraw the only minted beacon.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating a single swap, mint an extra beacon and withdraw it.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO is stored at a non-dApp address.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential alwaysSucceedValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The Swap UTxO is stored at a dApp address without staking.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) Nothing
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint an extra beacon and store it in the Swap UTxO.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 2
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has the wrong policy id.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = adaSymbol
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has the wrong beacon name.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = adaToken
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has the wrong offer asset policy id.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst askAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has the wrong offer asset name.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd askAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has the wrong ask asset policy id.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst offerAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has the wrong ask asset name.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd offerAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has a zero price.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 0 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` has a negative price.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio (-1) 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap UTxO is not stored with some of the offered asset.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Extra assets are stored in the Swap UTxO.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    <> uncurry singleton testToken15 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs, the first swap output is invalid. This test and 
-- `failureTest20` are to explicitly check that the order of swap outputs does not impact
-- transaction validity.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,2)]
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple Swap UTxOs, the second swap output is invalid. This test and 
-- `failureTest19` are to explicitly check that the order of swap outputs does not impact
-- transaction validity.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,2)]
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
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The swap price has a zero denominator.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconSym
        , unsafeBeaconName = assetBeacon
        , unsafeOfferId = fst offerAsset
        , unsafeOfferName = snd offerAsset
        , unsafeAskId = fst askAsset
        , unsafeAskName = snd askAsset
        , unsafeSwapPrice = (10,0)
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | The swap price has a negative denominator.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconSym
        , unsafeBeaconName = assetBeacon
        , unsafeOfferId = fst offerAsset
        , unsafeOfferName = snd offerAsset
        , unsafeAskId = fst askAsset
        , unsafeAskName = snd askAsset
        , unsafeSwapPrice = (10,-1)
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | `SwapDatum` is not an inline datum.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
                  [ ( Just $ TxOutDatumHash $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple swaps for the same offer asset but different ask assets, the beacons
-- are mixed up.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset1 = (adaSymbol,adaToken)
      askAsset2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon1 = genBeaconName askAsset1
      assetBeacon2 = genBeaconName askAsset2
      swapDatum1 = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon1
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon2
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 10 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset1,askAsset2]
              , mintTokens = [(assetBeacon1,1),(assetBeacon2,1)]
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
                    <> singleton beaconSym assetBeacon2 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym assetBeacon1 1
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When creating multiple swaps for different offer assets and different ask assets, the beacons
-- are mixed up.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      offerAsset2 = testToken3
      askAsset1 = (adaSymbol,adaToken)
      askAsset2 = testToken2
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon1 = genBeaconName askAsset1
      assetBeacon2 = genBeaconName askAsset2
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 10 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset1,offerAsset2]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset1
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset1]
              , mintTokens = [(assetBeacon1,1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset2
                  , Just (refScriptAddress, mintRefs!!1)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset2]
              , mintTokens = [(assetBeacon2,1)]
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
                    <> singleton beaconSym2 assetBeacon2 1
                    <> uncurry singleton offerAsset1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym1 assetBeacon1 1
                    <> uncurry singleton offerAsset2 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Mint beacons using the `BurnBeacons` redeemer.
failureTest26 :: EmulatorTrace ()
failureTest26 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
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
                    <> uncurry singleton offerAsset 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple Swap UTxOs for the same trading pair. The offered asset is a native asset.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon, fromIntegral numberCreated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberCreated
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for the same trading pair. The offered asset is ADA.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken1
      offerAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      assetBeacon = genBeaconName askAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon, fromIntegral numberCreated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberCreated
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10_000_000
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was a native token.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAssets = 
        take numberCreated $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [2..100]
      askBeacons = map genBeaconName askAssets
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = "" -- This will be replaced.
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = "" -- This will be replaced.
        , askName = "" -- This will be replaced.
        , swapPrice = unsafeRatio 1_000_000 1
        }

  let sampleOutputs = take numberCreated $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{askId = fst a, askName = snd a, beaconName = b}
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> uncurry singleton offerAsset 10
            )
          )
          askAssets
          askBeacons

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap askAssets
              , mintTokens = zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was ADA.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = ("","")
      askAssets = 
        take numberCreated $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [2..100]
      askBeacons = map genBeaconName askAssets
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym = beaconCurrencySymbol offerAsset
      swapDatum = SwapDatum
        { beaconId = beaconSym
        , beaconName = "" -- This will be replaced.
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = "" -- This will be replaced.
        , askName = "" -- This will be replaced.
        , swapPrice = unsafeRatio 1_000_000 1
        }

  let sampleOutputs = take numberCreated $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{askId = fst a, askName = snd a, beaconName = b}
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> uncurry singleton offerAsset 1_000_000
            )
          )
          askAssets
          askBeacons

  mintRefs <- initializeBeaconPolicies [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap askAssets
              , mintTokens = zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset 
-- is ADA.
benchTest5 :: Int -> EmulatorTrace ()
benchTest5 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = ("","")
      offerAssets = 
        take numberCreated $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..100]
      askBeacon = genBeaconName askAsset
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSyms = map beaconCurrencySymbol offerAssets
      swapDatum = SwapDatum
        { beaconId = "" -- This will be replaced
        , beaconName = askBeacon
        , offerId = "" -- This will be replaced.
        , offerName = "" -- This wil be replaced.
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies offerAssets

  let sampleOutputs = take numberCreated $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{offerId = fst a, offerName = snd a, beaconId = b}
            , lovelaceValueOf 3_000_000
            <> singleton b askBeacon 1
            <> uncurry singleton a 10
            )
          )
          offerAssets
          beaconSyms

  let sampleMints = take numberCreated $
        zipWith
          (\i o ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy o
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(askBeacon,1)]
              }
          )
          [0..]
          offerAssets

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset is
-- a native token.
benchTest6 :: Int -> EmulatorTrace ()
benchTest6 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken25
      offerAssets = 
        take numberCreated $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..100]
      askBeacon = genBeaconName askAsset
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSyms = map beaconCurrencySymbol offerAssets
      swapDatum = SwapDatum
        { beaconId = "" -- This will be replaced
        , beaconName = askBeacon
        , offerId = "" -- This will be replaced.
        , offerName = "" -- This wil be replaced.
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies offerAssets

  let sampleOutputs = take numberCreated $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{offerId = fst a, offerName = snd a, beaconId = b}
            , lovelaceValueOf 3_000_000
            <> singleton b askBeacon 1
            <> uncurry singleton a 10
            )
          )
          offerAssets
          beaconSyms

  let sampleMints = take numberCreated $
        zipWith
          (\i o ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy o
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(askBeacon,1)]
              }
          )
          [0..]
          offerAssets

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Create multiple Swap UTxOs for different offer assets and different ask assets.
benchTest7 :: Int -> EmulatorTrace ()
benchTest7 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets =
        take numberCreated $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      offerAssets = 
        take numberCreated $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
      askBeacons = map genBeaconName askAssets
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSyms = map beaconCurrencySymbol offerAssets
      swapDatum = SwapDatum
        { beaconId = "" -- This will be replaced.
        , beaconName = "" -- This will be replaced.
        , offerId = "" -- This will be replaced.
        , offerName = "" -- This wil be replaced.
        , askId = "" -- This will be replaced.
        , askName = "" -- This will be replaced.
        , swapPrice = unsafeRatio 1_000_000 1
        }

  mintRefs <- initializeBeaconPolicies offerAssets

  let sampleOutputs = take numberCreated $
        map 
          (\(ask,askBeacon,offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      , beaconName = askBeacon
                                      , askId = fst ask
                                      , askName = snd ask
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askBeacon 1
            <> uncurry singleton offer 10
            )
          )
          (zip4 askAssets askBeacons offerAssets beaconSyms)
          

  let sampleMints = take numberCreated $
        map
          (\(i,ask,askBeacon,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askBeacon,1)]
              }
          )
          (zip4 [0..] askAssets askBeacons offerAssets)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = sampleMints
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
benchTrace = runEmulatorTraceIO' def emConfig . benchTest3

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
        (assertEvaluationError "Only the beacons in the redeemer can be minted/burned") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Only the beacons in the redeemer can be minted/burned") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Only the beacons in the redeemer can be minted/burned") failureTest3
      -- Since the error message uses app_name, it cannot be hardcoded.
    , checkPredicateOptions opts "failureTest4"
        (Test.not assertNoFailedTransactions) failureTest4
      -- Since the error message uses app_name, it cannot be hardcoded.
    , checkPredicateOptions opts "failureTest5"
        (Test.not assertNoFailedTransactions) failureTest5
      -- Since the error message uses app_name, it cannot be hardcoded.
    , checkPredicateOptions opts "failureTest6"
        (Test.not assertNoFailedTransactions) failureTest6
      -- Since the error message uses app_name, it cannot be hardcoded.
    , checkPredicateOptions opts "failureTest7"
        (Test.not assertNoFailedTransactions) failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Beacons must be stored individually") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Invalid SwapDatum beacon_id") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Invalid SwapDatum offer_id") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Invalid SwapDatum offer_name") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Invalid SwapDatum ask_id") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Invalid SwapDatum ask_name") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Swap price not > 0") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Swap price not > 0") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Beacons must be stored with some of the offered asset") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "No extraneous assets can be stored in the swap UTxO") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Beacons must be stored with some of the offered asset") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Beacons must be stored with some of the offered asset") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Swap price denominator not > 0") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Swap price denominator not > 0") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "All swap datums must be inline datums") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "Beacons must be stored with some of the offered asset") failureTest25
    , checkPredicateOptions opts "failureTest26"
        (assertEvaluationError "This redeemer can only be used to burn") failureTest26

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 49
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 59
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 29
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 30
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 15
    , checkPredicateOptions opts "benchTest6"
        assertNoFailedTransactions $ benchTest6 15
    , checkPredicateOptions opts "benchTest7"
        assertNoFailedTransactions $ benchTest7 15

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 50
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 60
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 30
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 31
    , checkPredicateOptions opts "perfIncreaseTest5"
        (Test.not assertNoFailedTransactions) $ benchTest5 16
    , checkPredicateOptions opts "perfIncreaseTest6"
        (Test.not assertNoFailedTransactions) $ benchTest6 16
    , checkPredicateOptions opts "perfIncreaseTest7"
        (Test.not assertNoFailedTransactions) $ benchTest7 16
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig regressionTest7
