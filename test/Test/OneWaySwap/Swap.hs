{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

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

    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3

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
initializeScripts :: [(CurrencySymbol,TokenName)] -> EmulatorTrace ([TxOutRef],TxOutRef)
initializeScripts assets = do
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

  liftM2 (,) (mapM (txOutRefWithValue . lovelaceValueOf) idxs)
             (txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef)

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Execute a single swap. Mints another token to an unrelated output in the same transaction
-- to check that the validator correctly ignores other UTxOs.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. Each swap is from the same address and for the same trading pair. The
-- prices for the inputs are the same. Mints another token to an unrelated output in the same
-- transaction to check that the validator correctly ignores other UTxOs.
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 26_000_000 
                    <> singleton beaconSym assetBeacon 2
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. Each swap is from the same address and for the same trading pair. The
-- prices for the inputs are different. Mints another token to an unrelated output in the same
-- transaction to check that the validator correctly ignores other UTxOs.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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
                  , ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{swapPrice = unsafeRatio 2_000_000 1}
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{swapPrice = unsafeRatio 1_500_000 1}
                    , lovelaceValueOf 36_000_000 
                    <> singleton beaconSym assetBeacon 2
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. Each swap is from the same address and for the same ask asset but
-- different offer assets. Mints another token to an unrelated output in the same transaction to
-- check that the validator correctly ignores other UTxOs.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = ("","")
      offerAssets = 
        take 3 $
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

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = take 3 $
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

  let sampleMints = take 3 $
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

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let newSampleOutputs = take 3 $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{offerId = fst a, offerName = snd a, beaconId = b}
            , lovelaceValueOf 13_000_000
            <> singleton b askBeacon 1
            )
          )
          offerAssets
          beaconSyms

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = newSampleOutputs
              } 
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute mulitple swaps. Each swap is from the same address and for a different ask asset but
-- the same offer asset. Mints another token to an unrelated output in the same transaction to check
-- that the validator correclty ignores other UTxOs.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = (adaSymbol,adaToken)
      askAssets = 
        take 3 $
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
        , swapPrice = unsafeRatio 2 1
        }

  let sampleOutputs = take 3 $
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

  ( mintRefs,spendRef ) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  targets <- mapM txOutRefWithValue $ map snd sampleOutputs

  let newSampleOutputs = take 3 $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{askId = fst a, askName = snd a, beaconName = b}
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> singleton (fst a) (snd a) 20
            )
          )
          askAssets
          askBeacons

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = targets
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = newSampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps come from the same address and are chained together. Mints 
-- another token to an unrelated output in the same transaction to check that the validator 
-- correctly ignores other UTxOs.
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      askAsset1 = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      assetBeacon1 = genBeaconName askAsset1
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      offerAsset2 = testToken2
      askAsset2 = testToken1
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon2 = genBeaconName askAsset2
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 1 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset1,offerAsset2]

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym1 assetBeacon1 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym2 assetBeacon2 1
                    <> uncurry singleton offerAsset1 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. The swaps come from a different addresses and are chained together.
-- Mints another token to an unrelated output in the same transaction to check that the validator
-- correctly ignores other UTxOs.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let sellerCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      askAsset1 = (adaSymbol,adaToken)
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      assetBeacon1 = genBeaconName askAsset1
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      sellerCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      offerAsset2 = testToken2
      askAsset2 = testToken1
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon2 = genBeaconName askAsset2
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 1 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset1,offerAsset2]

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
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym1 assetBeacon1 1
                    <> uncurry singleton offerAsset1 10
                    )
                  ]
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
                  ( beaconMintingPolicy offerAsset2
                  , Just (refScriptAddress, mintRefs!!1)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset2]
              , mintTokens = [(assetBeacon2,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym2 assetBeacon2 1
                    <> uncurry singleton offerAsset2 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  swaps1 <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swaps2 <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr2

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = map fst swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr2
              , spendUtxos = map fst swaps2
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym1 assetBeacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym2 assetBeacon2 1
                    <> uncurry singleton offerAsset1 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute multiple swaps. Each swap is from a different address and for the same trading pair.
-- The prices for the inputs are different. Mints another token to an unrelated output in the same
-- transaction to check that the validator correctly ignores other UTxOs.
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
                 $ knownWallet 2
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr1 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred1)
      swapAddr2 = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred2)
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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
              { toAddress = swapAddr1
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

  void $ waitNSlots 2

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
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{swapPrice = unsafeRatio 2_000_000 1}
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
  
  void $ waitNSlots 2

  swaps1 <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr1
  swaps2 <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr2

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr1
              , spendUtxos = map fst swaps1
              }
          , ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr2
              , spendUtxos = map fst swaps2
              }
          ]
      , outputs = 
          [ 
            UtxoOutput
              { toAddress = swapAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = swapAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{swapPrice = unsafeRatio 2_000_000 1}
                    , lovelaceValueOf 23_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When the asked asset is not ADA, ADA can still be deposited in case the minimum required
-- ADA amount for the UTxO increases.
regressionTest9 :: EmulatorTrace ()
regressionTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken1
      offerAsset = testToken2
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
        , swapPrice = unsafeRatio 1 1_000_000
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton askAsset 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | Swap input does not have a beacon.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_000 
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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 13_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap beacon withdrawn from swap address.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 23_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap without depositing enough of the asked asset.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 6_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong beacon id.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{beaconId = adaSymbol}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong beacon name.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{beaconName = ""}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong offer id.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{offerId = beaconSym}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong offer name.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{offerName = "Test"}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong ask id.
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{askId = beaconSym}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong ask name.
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
        { beaconId = beaconSym
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{askName = "Test"}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, the output's `SwapDatum` has the wrong swap price.
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
        , beaconName = assetBeacon
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 0 1}
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for the same trading pair and from the same address, the output
-- datum's swap price is not the weighted average of the inputs.
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
        , offerId = fst offerAsset
        , offerName = snd offerAsset
        , askId = fst askAsset
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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
                  , ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum{swapPrice = unsafeRatio 2_000_000 1}
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum swapDatum
                    , lovelaceValueOf 36_000_000 
                    <> singleton beaconSym assetBeacon 2
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for different trading pairs, the beacons are mixed up in the
-- outputs.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      askAsset1 = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      assetBeacon1 = genBeaconName askAsset1
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      offerAsset2 = testToken2
      askAsset2 = testToken1
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon2 = genBeaconName askAsset2
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 1 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset1,offerAsset2]

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym2 assetBeacon2 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym1 assetBeacon1 1
                    <> uncurry singleton offerAsset1 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing multiple swaps for different trading pairs but from the same address, the 
-- beacons are combined in the outputs.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset1 = testToken1
      askAsset1 = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSym1 = beaconCurrencySymbol offerAsset1
      assetBeacon1 = genBeaconName askAsset1
      swapDatum1 = SwapDatum
        { beaconId = beaconSym1
        , beaconName = assetBeacon1
        , offerId = fst offerAsset1
        , offerName = snd offerAsset1
        , askId = fst askAsset1
        , askName = snd askAsset1
        , swapPrice = unsafeRatio 1_000_000 1
        }
      offerAsset2 = testToken2
      askAsset2 = testToken1
      beaconSym2 = beaconCurrencySymbol offerAsset2
      assetBeacon2 = genBeaconName askAsset2
      swapDatum2 = SwapDatum
        { beaconId = beaconSym2
        , beaconName = assetBeacon2
        , offerId = fst offerAsset2
        , offerName = snd offerAsset2
        , askId = fst askAsset2
        , askName = snd askAsset2
        , swapPrice = unsafeRatio 1 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset1,offerAsset2]

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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum1
                    , lovelaceValueOf 16_000_000 
                    <> singleton beaconSym1 assetBeacon1 1
                    <> singleton beaconSym2 assetBeacon2 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When executing a single swap, an extraneous asset is added to the swap UTxO.
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
        , askName = snd askAsset
        , swapPrice = unsafeRatio 1_000_000 1
        }

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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

  void $ waitNSlots 2

  swap <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconSym assetBeacon 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = [ swap ]
              }
          ]
      , outputs = 
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 13_000_000 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton testToken13 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Swap multiple Swap UTxOs for the same trading pair and from the same address. The swap change
-- was consolidated into a single output.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberSwapped = do
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]

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
              , mintTokens = [(assetBeacon, 20)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 20
                  ( Just $ TxOutDatumInline $ toDatum swapDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Swap
              , spendFromAddress = swapAddr
              , spendUtxos = take numberSwapped $ map fst swaps
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf (13_000_000 * fromIntegral numberSwapped)
                    <> singleton beaconSym assetBeacon (fromIntegral numberSwapped)
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Swap multiple Swap UTxOs for the same trading pair but from different addresses. 
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberSwapped = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h5 <- activateContractWallet (knownWallet 5) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints
  h7 <- activateContractWallet (knownWallet 7) endpoints
  h8 <- activateContractWallet (knownWallet 8) endpoints
  h9 <- activateContractWallet (knownWallet 9) endpoints
  h10 <- activateContractWallet (knownWallet 10) endpoints

  let sellerCred = PubKeyCredential
                 . unPaymentPubKeyHash 
                 . mockWalletPaymentPubKeyHash 
                 . knownWallet
      offerAsset = testToken1
      askAsset = (adaSymbol,adaToken)
      swapAddr = Address (ScriptCredential swapValidatorHash) . Just .  StakingHash . sellerCred
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

  (mintRefs,spendRef) <- initializeScripts [offerAsset]
  
  zipWithM_
    (\h cred -> do
      callEndpoint @"create-transaction" h $
        CreateTransactionParams
          { tokens = 
              [ 
                TokenMint 
                  { mintWitness = 
                      ( beaconMintingPolicy offerAsset
                      , Just (refScriptAddress, mintRefs!!0)
                      )
                  , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
                  , mintTokens = [(assetBeacon, 1)]
                  }
              ]
          , inputs = []
          , outputs =
              [ UtxoOutput
                  { toAddress = swapAddr cred
                  , outputUtxos =
                      [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                        , lovelaceValueOf 3_000_000 
                        <> singleton beaconSym assetBeacon 1
                        <> uncurry singleton offerAsset 10
                        )
                      ]
                  }
              ]
          , validityRange = ValidityInterval Nothing Nothing
          }

      void $ waitNSlots 2
    )
    [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10]
    [1..10]

  swaps <- concat <$> mapM (txOutRefsAndDatumsAtAddress @SwapDatum . swapAddr) [1..10]

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          take numberSwapped $ zipWith 
            (\(ref,_) cred ->
              ScriptUtxoInput
                { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
                , spendRedeemer = toRedeemer Swap
                , spendFromAddress = swapAddr cred
                , spendUtxos = [ ref ]
                }
            )
            swaps
            [1..10]
      , outputs = 
          take numberSwapped $ map
            (\cred ->
              UtxoOutput
                { toAddress = swapAddr cred
                , outputUtxos = 
                    [ ( Just $ TxOutDatumInline $ toDatum swapDatum
                      , lovelaceValueOf 13_000_000 
                      <> singleton beaconSym assetBeacon 1
                      )
                    ]
                }
            )
            [1..10]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Execute mulitple Swap UTxOs for different trading pairs but from the same address.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberSwapped = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = (adaSymbol,adaToken)
      offerAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..40]
      askBeacon = genBeaconName askAsset
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSyms = map beaconCurrencySymbol offerAssets
      swapDatum = SwapDatum
        { beaconId = "" -- This will be replaced.
        , beaconName = askBeacon
        , offerId = "" -- This will be replaced.
        , offerName = "" -- This wil be replaced.
        , askId = "" 
        , askName = "" 
        , swapPrice = unsafeRatio 1_000_000 1
        }

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = 
        map 
          (\(offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askBeacon 1
            <> uncurry singleton offer 10
            )
          )
          (zip offerAssets beaconSyms)

  let sampleMints = 
        map
          (\(i,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(askBeacon,1)]
              }
          )
          (zip [0..] offerAssets)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 15 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 15 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberSwapped <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let newSampleOutputs = 
        map 
          (\(_,Just d) ->
            ( Just $ TxOutDatumInline
                   $ toDatum d
            , lovelaceValueOf 3_000_000
            <> singleton (beaconId d) askBeacon 1
            <> singleton (askId d) (askName d) 10_000_000
            )
          )
          swaps

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = []
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
              , outputUtxos = newSampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest3

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

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Swap input missing swap beacon") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Fail: offer_taken * weighted_price <= ask_given") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Fail: offer_taken * weighted_price <= ask_given") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Invalid SwapDatum beacon_id") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Invalid SwapDatum offer_id") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Invalid SwapDatum offer_name") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Invalid SwapDatum ask_id") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Invalid SwapDatum ask_name") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "SwapDatum price not weighted avg") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "SwapDatum price not weighted avg") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Invalid SwapDatum beacon_id") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Only the asked asset or ADA can be deposited into the swap") failureTest14

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 5
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 9
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 9

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 6
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 10
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 10
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig regressionTest9
