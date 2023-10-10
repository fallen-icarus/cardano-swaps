{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

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
  , regressionTest7
  , regressionTest8
  , regressionTest9
  , regressionTest10
  , regressionTest11
  , regressionTest12
  , regressionTest13
  , regressionTest14
  , regressionTest15
  , regressionTest16
  , regressionTest17
  , regressionTest18
  , regressionTest19

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
  , benchTest6
  , benchTest7
  , benchTest8
  , benchTest9
  , benchTest10
  , benchTest11
  , benchTest12
  , benchTest13
  , benchTest14
  , benchTest15
  , benchTest16

    -- * Full test function
  , tests
  ) where

import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.String (fromString)
import Data.List (zip4,elemIndex)

import Test.Internal
import Test.Config
import Test.OneWaySwap.Utils
import CardanoSwaps.OneWaySwap
import CardanoSwaps.Utils

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
-- | Close a single swap UTxO. Mints an unrelated token to an unrelated output in the same
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
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update a single swap UTxO. Mints an unrelated token to an unrelated output in the same
-- transaction to also check if the beacon policy can correctly ignore unrelated tokens and UTxOs.
-- The offered asset is a native token and the asked asset is ADA.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
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

-- | Close multiple Swap UTxOs for the same trading pair. The offered asset is a native asset.
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
              , mintTokens = [(assetBeacon, 3)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,-3)]
              }
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
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same trading pair. The offered asset is a native asset.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
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
              , mintTokens = [(assetBeacon, 3)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for the same trading pair. The offered asset is ADA.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
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
              , mintTokens = [(assetBeacon, 3)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,-3)]
              }
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
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same trading pair. The offered asset is ADA.
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
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
              , mintTokens = [(assetBeacon, 3)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was a native token.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
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
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = zip askBeacons $ repeat (-1)
              }
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
              , spendUtxos = targets
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was a native token.
regressionTest8 :: EmulatorTrace ()
regressionTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
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
        , swapPrice = unsafeRatio 1_000_000 1
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
                   $ toDatum swapDatum{ askId = fst a
                                      , askName = snd a
                                      , beaconName = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
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

-- | Close multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was ADA.
regressionTest9 :: EmulatorTrace ()
regressionTest9 = do
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
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = zip askBeacons $ repeat (-1)
              }
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
              , spendUtxos = targets
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was ADA.
regressionTest10 :: EmulatorTrace ()
regressionTest10 = do
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
        , swapPrice = unsafeRatio 1_000_000 1
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
                   $ toDatum swapDatum{ askId = fst a
                                      , askName = snd a
                                      , beaconName = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> uncurry singleton offerAsset 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
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

-- | Close multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset 
-- is ADA.
regressionTest11 :: EmulatorTrace ()
regressionTest11 = do
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

  let sampleBurns = take 3 $
        zipWith
          (\i o ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy o
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(askBeacon,-1)]
              }
          )
          [0..]
          offerAssets

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = mconcat
          [ sampleBurns
          , [ 
              TokenMint 
                { mintWitness =
                    ( alwaysSucceedPolicy
                    , Nothing
                    )
                , mintRedeemer = toRedeemer ()
                , mintTokens = [("Other",1)]
                }
            ]
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

-- | Close multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset 
-- is a native token.
regressionTest12 :: EmulatorTrace ()
regressionTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken1
      offerAssets = 
        take 3 $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [2..100]
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

  let sampleBurns = take 3 $
        zipWith
          (\i o ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy o
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(askBeacon,-1)]
              }
          )
          [0..]
          offerAssets

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = mconcat
          [ sampleBurns
          , [ 
              TokenMint 
                { mintWitness =
                    ( alwaysSucceedPolicy
                    , Nothing
                    )
                , mintRedeemer = toRedeemer ()
                , mintTokens = [("Other",1)]
                }
            ]
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

-- | Update multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset 
-- is ADA.
regressionTest13 :: EmulatorTrace ()
regressionTest13 = do
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
                   $ toDatum swapDatum{ offerId = fst a
                                      , offerName = snd a
                                      , beaconId = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton b askBeacon 1
            <> uncurry singleton a 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
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

-- | Update multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset 
-- is ADA.
regressionTest14 :: EmulatorTrace ()
regressionTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = testToken1
      offerAssets = 
        take 3 $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [2..100]
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
                   $ toDatum swapDatum{ offerId = fst a
                                      , offerName = snd a
                                      , beaconId = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton b askBeacon 1
            <> uncurry singleton a 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst swaps
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

-- | Close multiple Swap UTxOs for different offer assets and different ask assets.
regressionTest15 :: EmulatorTrace ()
regressionTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets =
        take 3 $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      offerAssets = 
        take 3 $
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

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = take 3 $
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
          

  let sampleMints = take 3 $
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

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let sampleBurns = take 3 $
        map
          (\(i,ask,askBeacon,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askBeacon,-1)]
              }
          )
          (zip4 [0..] askAssets askBeacons offerAssets)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = mconcat
          [ sampleBurns
          , [ 
              TokenMint 
                { mintWitness =
                    ( alwaysSucceedPolicy
                    , Nothing
                    )
                , mintRedeemer = toRedeemer ()
                , mintTokens = [("Other",1)]
                }
            ]
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

-- | Update multiple Swap UTxOs for different offer assets and different ask assets.
regressionTest16 :: EmulatorTrace ()
regressionTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets =
        take 3 $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      offerAssets = 
        take 3 $
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

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = take 3 $
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
          

  let sampleMints = take 3 $
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

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let newSampleOutputs = take 3 $
        map 
          (\(ask,askBeacon,offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      , beaconName = askBeacon
                                      , askId = fst ask
                                      , askName = snd ask
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askBeacon 1
            <> uncurry singleton offer 10
            )
          )
          (zip4 askAssets askBeacons offerAssets beaconSyms)

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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = newSampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose the `CreateSwap` and `CloseOrUpdate` redeemer to change what trading pair a single
-- Swap UTxO is for.
regressionTest17 :: EmulatorTrace ()
regressionTest17 = do
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

  let newAskAsset = testToken2
      newBeaconName = genBeaconName testToken2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset,newAskAsset]
              , mintTokens = [(assetBeacon,-1),(newBeaconName,1)]
              }
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
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName
                                                                 , askId = fst newAskAsset
                                                                 , askName = snd newAskAsset
                                                                 }
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym newBeaconName 1
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

-- | Compose the `CreateSwap` and `CloseOrUpdate` redeemer to change what trading pair multiple
-- Swap UTxOs are for. The Swap UTxOs start as the same trading pair and are changed to the same
-- trading pair.
regressionTest18 :: EmulatorTrace ()
regressionTest18 = do
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
                    , lovelaceValueOf 3_000_001 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_002
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

  swap1 <- txOutRefWithValue $ 
    lovelaceValueOf 3_000_001
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10
  swap2 <- txOutRefWithValue $
    lovelaceValueOf 3_000_002
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10

  let newAskAsset = testToken2
      newBeaconName = genBeaconName testToken2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset,newAskAsset]
              , mintTokens = [(assetBeacon,-2),(newBeaconName,2)]
              }
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
              , spendUtxos = [ swap1,swap2 ]
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName
                                                                 , askId = fst newAskAsset
                                                                 , askName = snd newAskAsset
                                                                 }
                    , lovelaceValueOf 3_000_001
                    <> singleton beaconSym newBeaconName 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName
                                                                 , askId = fst newAskAsset
                                                                 , askName = snd newAskAsset
                                                                 }
                    , lovelaceValueOf 3_000_002
                    <> singleton beaconSym newBeaconName 1
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

-- | Compose the `CreateSwap` and `CloseOrUpdate` redeemer to change what trading pair multiple
-- Swap UTxOs are for. The Swap UTxOs start as the same trading pair and are changed to different
-- trading pairs.
regressionTest19 :: EmulatorTrace ()
regressionTest19 = do
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
                    , lovelaceValueOf 3_000_001 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_002
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

  swap1 <- txOutRefWithValue $ 
    lovelaceValueOf 3_000_001
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10
  swap2 <- txOutRefWithValue $
    lovelaceValueOf 3_000_002
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10

  let newAskAsset1 = testToken2
      newBeaconName1 = genBeaconName newAskAsset1
      newAskAsset2 = testToken3
      newBeaconName2 = genBeaconName newAskAsset2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset,newAskAsset1,newAskAsset2]
              , mintTokens = [(assetBeacon,-2),(newBeaconName1,1),(newBeaconName2,1)]
              }
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
              , spendUtxos = [ swap1,swap2 ]
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName1
                                                                 , askId = fst newAskAsset1
                                                                 , askName = snd newAskAsset1
                                                                 }
                    , lovelaceValueOf 3_000_001
                    <> singleton beaconSym newBeaconName1 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName2
                                                                 , askId = fst newAskAsset2
                                                                 , askName = snd newAskAsset2
                                                                 }
                    , lovelaceValueOf 3_000_002
                    <> singleton beaconSym newBeaconName2 1
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
-- Failure Tests
-------------------------------------------------
-- | When closing a single swap UTxO, withdraw the beacon.
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
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating a single swap UTxO, withdraw the beacon.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
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

-- | When closing multiple swap UTxOs for the same trading pair, not all of the beacons were
-- burned.
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
              , mintTokens = [(assetBeacon, 3)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,-2)]
              }
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
              , spendUtxos = map fst swaps
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing multiple swap UTxOs for the same offer asset but different ask assets, not all
-- beacons are burned.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
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
        , swapPrice = unsafeRatio 1_000_000 1
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
              , mintTokens = take 2 $ zip askBeacons $ repeat (-1)
              }
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
              , spendUtxos = targets
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When closing multiple swap UTxOs for different offer and ask assets, not all beacons are
-- burned.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets =
        take 3 $
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [21..40]
      offerAssets = 
        take 3 $
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

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = take 3 $
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
          

  let sampleMints = take 3 $
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

  void $ waitNSlots 2

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let sampleBurns = take 3 $
        map
          (\(i,ask,askBeacon,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askBeacon,-1)]
              }
          )
          (zip4 [0..] askAssets askBeacons offerAssets)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = mconcat
          [ take 2 $ sampleBurns
          , [ 
              TokenMint 
                { mintWitness =
                    ( alwaysSucceedPolicy
                    , Nothing
                    )
                , mintRedeemer = toRedeemer ()
                , mintTokens = [("Other",1)]
                }
            ]
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

-- | When updating a single swap, the new price is negative.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio (-10) 1}
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

-- | When updating a single swap, the new price is zero.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 0 1}
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

-- | When updating a single swap, the new price has a zero denominator.
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

  let newSwapDatum = UnsafeDatum
        { unsafeBeaconId = beaconId swapDatum
        , unsafeBeaconName = beaconName swapDatum
        , unsafeOfferId = offerId swapDatum
        , unsafeOfferName = offerName swapDatum
        , unsafeAskId = askId swapDatum
        , unsafeAskName = askName swapDatum
        , unsafeSwapPrice = (10,0)
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newSwapDatum
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

-- | When updating a single swap, the new price has a negative denominator.
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

  let newSwapDatum = UnsafeDatum
        { unsafeBeaconId = beaconId swapDatum
        , unsafeBeaconName = beaconName swapDatum
        , unsafeOfferId = offerId swapDatum
        , unsafeOfferName = offerName swapDatum
        , unsafeAskId = askId swapDatum
        , unsafeAskName = askName swapDatum
        , unsafeSwapPrice = (10,-1)
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newSwapDatum
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

-- | When updating a single swap, the swap UTxO is not stored with the offer asset.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
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

-- | When updating multiple swap UTxOs for the same trading pair, group up the beacons into the 
-- same output UTxO.
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
              , mintTokens = [(assetBeacon, 3)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 3
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 1
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 3
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | When updating multiple swap UTxOs for different trading pairs, mix up the beacons in the
-- outputs.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
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
        , swapPrice = unsafeRatio 1_000_000 1
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
        map 
          (\(a,b,b') ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ askId = fst a
                                      , askName = snd a
                                      , beaconName = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b' 1
            <> uncurry singleton offerAsset 10
            )
          )
          (zip3 askAssets askBeacons $ reverse askBeacons)

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

-- | When updating a single swap, the beacon symbol is changed.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ swapPrice = unsafeRatio 10 1
                                                                 , beaconId = adaSymbol
                                                                 }
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

-- | When updating a single swap, the beacon name is changed.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ swapPrice = unsafeRatio 10 1
                                                                 , beaconName = adaToken
                                                                 }
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

-- | When updating a single swap, the offer id is changed.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ swapPrice = unsafeRatio 10 1
                                                                 , offerId = beaconSym
                                                                 }
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

-- | When updating a single swap, the offer name is changed.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ swapPrice = unsafeRatio 10 1
                                                                 , offerName = "other"
                                                                 }
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

-- | When updating a single swap, the ask id is changed.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ swapPrice = unsafeRatio 10 1
                                                                 , askId = beaconSym
                                                                 }
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

-- | When updating a single swap, the ask name is changed.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ swapPrice = unsafeRatio 10 1
                                                                 , askName = "other"
                                                                 }
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

-- | When updating a single swap, store an extraneous asset in the swap UTxO.
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
          [ TokenMint 
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
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

-- | When updating multiple Swap UTxOs, the first output is invalid. This test and `failureTest21`
-- are to explicitly check that the order of transaction outputs does not impact transaction
-- validity.
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
                    , lovelaceValueOf 3_000_001 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_002
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

  swap1 <- txOutRefWithValue $ 
    lovelaceValueOf 3_000_001
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10
  swap2 <- txOutRefWithValue $
    lovelaceValueOf 3_000_002
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10

  let newAskAsset1 = testToken2
      newBeaconName1 = genBeaconName newAskAsset1
      newAskAsset2 = testToken3
      newBeaconName2 = genBeaconName newAskAsset2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset,newAskAsset1,newAskAsset2]
              , mintTokens = [(assetBeacon,-2),(newBeaconName1,1),(newBeaconName2,1)]
              }
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
              , spendUtxos = [ swap1,swap2 ]
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName1
                                                                 , askId = fst newAskAsset1
                                                                 , askName = snd newAskAsset1
                                                                 }
                    , lovelaceValueOf 3_000_001
                    <> singleton beaconSym newBeaconName1 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName2
                                                                 , askId = fst newAskAsset2
                                                                 , askName = snd newAskAsset1
                                                                 }
                    , lovelaceValueOf 3_000_002
                    <> singleton beaconSym newBeaconName2 1
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

-- | When updating multiple Swap UTxOs, the first output is invalid. This test and `failureTest20`
-- are to explicitly check that the order of transaction outputs does not impact transaction
-- validity.
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
                    , lovelaceValueOf 3_000_001 
                    <> singleton beaconSym assetBeacon 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum
                    , lovelaceValueOf 3_000_002
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

  swap1 <- txOutRefWithValue $ 
    lovelaceValueOf 3_000_001
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10
  swap2 <- txOutRefWithValue $
    lovelaceValueOf 3_000_002
    <> singleton beaconSym assetBeacon 1
    <> uncurry singleton offerAsset 10

  let newAskAsset1 = testToken2
      newBeaconName1 = genBeaconName newAskAsset1
      newAskAsset2 = testToken3
      newBeaconName2 = genBeaconName newAskAsset2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset,newAskAsset1,newAskAsset2]
              , mintTokens = [(assetBeacon,-2),(newBeaconName1,1),(newBeaconName2,1)]
              }
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
              , spendUtxos = [ swap1,swap2 ]
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName1
                                                                 , askId = fst newAskAsset1
                                                                 , askName = snd newAskAsset2
                                                                 }
                    , lovelaceValueOf 3_000_001
                    <> singleton beaconSym newBeaconName1 1
                    <> uncurry singleton offerAsset 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName2
                                                                 , askId = fst newAskAsset2
                                                                 , askName = snd newAskAsset2
                                                                 }
                    , lovelaceValueOf 3_000_002
                    <> singleton beaconSym newBeaconName2 1
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

-- | Fail if old beacon not burned when composing `CreateSwap` with `CloseOrUpdate`.
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

  let newAskAsset = testToken2
      newBeaconName = genBeaconName testToken2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [newAskAsset]
              , mintTokens = [(assetBeacon,0),(newBeaconName,1)]
              }
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
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName
                                                                 , askId = fst newAskAsset
                                                                 , askName = snd newAskAsset
                                                                 }
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym newBeaconName 1
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

-- | When composing `CreateSwap` and `CloseOrUpdate` the old asset is not present in the 
-- `CreateSwap` redeemer.
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

  let newAskAsset = testToken2
      newBeaconName = genBeaconName testToken2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [newAskAsset]
              , mintTokens = [(assetBeacon,-1),(newBeaconName,1)]
              }
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
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName
                                                                 , askId = fst newAskAsset
                                                                 , askName = snd newAskAsset
                                                                 }
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconSym newBeaconName 1
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

-- | Address owner did not approve.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
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
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple Swap UTxOs for the same trading pair. The offered asset is a native asset.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberClosed = do
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
              , mintTokens = [(assetBeacon, 48)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 48
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
              , mintTokens = [(assetBeacon, 48)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 48
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,fromIntegral (-numberClosed))]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = take numberClosed $ map fst swaps
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same trading pair. The offered asset is a native asset.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberUpdated = do
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
              , mintTokens = [(assetBeacon, fromIntegral numberUpdated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberUpdated
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
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = take numberUpdated $ map fst swaps
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberUpdated
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for the same trading pair. The offered asset is ADA.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberClosed = do
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
              , mintTokens = [(assetBeacon, 48)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 48
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
              , mintTokens = [(assetBeacon, 48)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate 48
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(assetBeacon,fromIntegral (-numberClosed))]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = take numberClosed $ map fst swaps
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same trading pair. The offered asset is ADA.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberUpdated = do
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
              , mintTokens = [(assetBeacon, fromIntegral numberUpdated)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberUpdated
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset]
              , mintTokens = [(assetBeacon,0)]
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = take numberUpdated $ map fst swaps
              }
          ]
      , outputs = 
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberUpdated
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{swapPrice = unsafeRatio 10 1}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconSym assetBeacon 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was a native token.
benchTest5 :: Int -> EmulatorTrace ()
benchTest5 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAssets = 
        take numberClosed $
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

  let sampleOutputs = take numberClosed $
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
              , mintRedeemer = toRedeemer $ CreateSwap $ take 25 askAssets
              , mintTokens = take 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap $ drop 25 askAssets
              , mintTokens = drop 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = drop 25 $ sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2 

  targets <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
              , mintTokens = zip askBeacons $ repeat (-1)
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst targets
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was ADA.
benchTest6 :: Int -> EmulatorTrace ()
benchTest6 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = (adaSymbol,adaToken)
      askAssets = 
        take numberClosed $
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

  let sampleOutputs = take numberClosed $
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
              , mintRedeemer = toRedeemer $ CreateSwap $ take 25 askAssets
              , mintTokens = take 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap $ drop 25 askAssets
              , mintTokens = drop 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = drop 25 $ sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2 

  targets <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

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
              , mintTokens = zip askBeacons $ repeat (-1)
              }
          ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst targets
              }
          ]
      , outputs = []
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Update multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was a native token.
benchTest7 :: Int -> EmulatorTrace ()
benchTest7 numberUpdated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = testToken1
      askAssets = 
        take numberUpdated $
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

  let sampleOutputs = take numberUpdated $
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
              , mintRedeemer = toRedeemer $ CreateSwap $ take 25 askAssets
              , mintTokens = take 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap $ drop 25 askAssets
              , mintTokens = drop 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = drop 25 $ sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targets <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  let newSampleOutputs = take numberUpdated $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ askId = fst a
                                      , askName = snd a
                                      , beaconName = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> uncurry singleton offerAsset 10
            )
          )
          askAssets
          askBeacons

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst targets
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

-- | Update multiple Swap UTxOs for the same offer asset but different ask assets. The offer asset
-- was ADA.
benchTest8 :: Int -> EmulatorTrace ()
benchTest8 numberUpdated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      offerAsset = (adaSymbol,adaToken)
      askAssets = 
        take numberUpdated $
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

  let sampleOutputs = take numberUpdated $
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
              , mintRedeemer = toRedeemer $ CreateSwap $ take 25 askAssets
              , mintTokens = take 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 25 $ sampleOutputs
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
              , mintRedeemer = toRedeemer $ CreateSwap $ drop 25 askAssets
              , mintTokens = drop 25 $ zip askBeacons $ repeat 1
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = drop 25 $ sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targets <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddr

  let newSampleOutputs = take numberUpdated $
        zipWith 
          (\a b ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ askId = fst a
                                      , askName = snd a
                                      , beaconName = b
                                      , swapPrice = unsafeRatio 11 1
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym b 1
            <> uncurry singleton offerAsset 10
            )
          )
          askAssets
          askBeacons

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [
            ScriptUtxoInput
              { spendWitness = (swapValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOrUpdate
              , spendFromAddress = swapAddr
              , spendUtxos = map fst targets
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

-- | Close multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset
-- is ADA.
benchTest9 :: Int -> EmulatorTrace ()
benchTest9 numberClosed = do
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
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 20 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 30 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 30 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberClosed <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let sampleBurns = 
        map
          (\(_,Just d) ->
            let offer = (offerId d,offerName d)
                Just i = elemIndex offer offerAssets
            in
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy offer
                    , Just (refScriptAddress, mintRefs!!i)
                    )
                , mintRedeemer = toRedeemer BurnBeacons 
                , mintTokens = [(beaconName d,-1)]
                }
          )
          swaps

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
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Close multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset
-- is a native asset.
benchTest10 :: Int -> EmulatorTrace ()
benchTest10 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = (fst testToken2,"TestToken100")
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
        , askId = fst askAsset
        , askName = snd askAsset 
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
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 20 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 30 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 30 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberClosed <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let sampleBurns = 
        map
          (\(_,Just d) ->
            let offer = (offerId d,offerName d)
                Just i = elemIndex offer offerAssets
            in
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy offer
                    , Just (refScriptAddress, mintRefs!!i)
                    )
                , mintRedeemer = toRedeemer BurnBeacons 
                , mintTokens = [(beaconName d,-1)]
                }
          )
          swaps

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
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Updating multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset
-- is ADA.
benchTest11 :: Int -> EmulatorTrace ()
benchTest11 numberUpdated = do
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
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 20 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 30 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 30 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberUpdated <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let newSampleOutputs = 
        map
          (\(_,Just d) ->
              ( Just $ TxOutDatumInline
                     $ toDatum d{swapPrice = unsafeRatio 11 1}
              , lovelaceValueOf 3_000_000
              <> singleton (beaconId d) askBeacon 1
              <> singleton (offerId d) (offerName d) 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
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

-- | Updating multiple Swap UTxOs for different offer assets but the same ask asset. The ask asset
-- is a native token.
benchTest12 :: Int -> EmulatorTrace ()
benchTest12 numberUpdated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAsset = (fst testToken2,"TestToken100")
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
        , askId = fst askAsset
        , askName = snd askAsset
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
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 20 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 30 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 30 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberUpdated <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let newSampleOutputs = 
        map
          (\(_,Just d) ->
              ( Just $ TxOutDatumInline
                     $ toDatum d{swapPrice = unsafeRatio 11 1}
              , lovelaceValueOf 3_000_000
              <> singleton (beaconId d) askBeacon 1
              <> singleton (offerId d) (offerName d) 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
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

-- | Close multiple Swap UTxOs for different offer assets and different ask assets. 
benchTest13 :: Int -> EmulatorTrace ()
benchTest13 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [41..80]
      offerAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..40]
      askBeacons = map genBeaconName askAssets
      swapAddr = Address (ScriptCredential swapValidatorHash) (Just $ StakingHash sellerCred)
      beaconSyms = map beaconCurrencySymbol offerAssets
      swapDatum = SwapDatum
        { beaconId = "" -- This will be replaced.
        , beaconName = "" -- This will get replaced.
        , offerId = "" -- This will be replaced.
        , offerName = "" -- This wil be replaced.
        , askId = "" -- This will get replaced.
        , askName = "" -- This will get replaced.
        , swapPrice = unsafeRatio 1_000_000 1
        }

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = 
        map 
          (\(ask,askSym,offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      , beaconName = askSym
                                      , askId = fst ask
                                      , askName = snd ask
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askSym 1
            <> uncurry singleton offer 10
            )
          )
          (zip4 askAssets askBeacons offerAssets beaconSyms)

  let sampleMints = 
        map
          (\(i,ask,askSym,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askSym,1)]
              }
          )
          (zip4 [0..] askAssets askBeacons offerAssets)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 20 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 30 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 30 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberClosed <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let sampleBurns = 
        map
          (\(_,Just d) ->
            let offer = (offerId d,offerName d)
                Just i = elemIndex offer offerAssets
            in
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy offer
                    , Just (refScriptAddress, mintRefs!!i)
                    )
                , mintRedeemer = toRedeemer BurnBeacons 
                , mintTokens = [(beaconName d,-1)]
                }
          )
          swaps

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
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Updating multiple Swap UTxOs for different offer assets and different ask assets.
benchTest14 :: Int -> EmulatorTrace ()
benchTest14 numberUpdated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [41..80]
      offerAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..40]
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

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let sampleOutputs = 
        map 
          (\(ask,askSym,offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      , beaconName = askSym
                                      , askId = fst ask
                                      , askName = snd ask
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askSym 1
            <> uncurry singleton offer 10
            )
          )
          (zip4 askAssets askBeacons offerAssets beaconSyms)

  let sampleMints = 
        map
          (\(i,ask,askSym,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askSym,1)]
              }
          )
          (zip4 [0..] askAssets askBeacons offerAssets)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 20 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 30 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 30 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberUpdated <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let newSampleOutputs = 
        map
          (\(_,Just d) ->
              ( Just $ TxOutDatumInline
                     $ toDatum d{swapPrice = unsafeRatio 11 1}
              , lovelaceValueOf 3_000_000
              <> singleton (beaconId d) (beaconName d) 1
              <> singleton (offerId d) (offerName d) 10
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
              , spendRedeemer = toRedeemer CloseOrUpdate
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

-- | Compose the `CreateSwap` and `CloseOrUpdate` redeemer to change what trading pair multiple
-- Swap UTxOs are for. The Swap UTxOs start as the same trading pair and are changed to the same
-- trading pair. The offer asset is the same before and after.
benchTest15 :: Int -> EmulatorTrace ()
benchTest15 numberChanged = do
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
              , mintTokens = [(assetBeacon,fromIntegral numberChanged)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = 
                  replicate numberChanged 
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

  let newAskAsset = testToken2
      newBeaconName = genBeaconName testToken2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [askAsset,newAskAsset]
              , mintTokens = 
                  [ (assetBeacon, fromIntegral (-numberChanged))
                  , (newBeaconName, fromIntegral numberChanged)
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = replicate numberChanged 
                  ( Just $ TxOutDatumInline $ toDatum swapDatum{ beaconName = newBeaconName
                                                               , askId = fst newAskAsset
                                                               , askName = snd newAskAsset
                                                               }
                  , lovelaceValueOf 3_000_000
                  <> singleton beaconSym newBeaconName 1
                  <> uncurry singleton offerAsset 10
                  )
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compose the `CreateSwap` and `CloseOrUpdate` redeemer to change what trading pair multiple
-- Swap UTxOs are for. The Swap UTxOs start as the same trading pair and are changed to different
-- trading pairs. The offer asset remains the same before and after.
benchTest16 :: Int -> EmulatorTrace ()
benchTest16 numberChanged = do
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
              , mintTokens = [(assetBeacon,fromIntegral numberChanged)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos =  replicate numberChanged
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

  let newAskAssets = take numberChanged $ 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [41..80]
      newBeaconNames = map genBeaconName newAskAssets

  let sampleOutputs = 
        map 
          (\(ask,askSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offerAsset
                                      , offerName = snd offerAsset
                                      , beaconId = beaconSym
                                      , beaconName = askSym
                                      , askId = fst ask
                                      , askName = snd ask
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askSym 1
            <> uncurry singleton offerAsset 10
            )
          )
          (zip newAskAssets newBeaconNames)

  let sampleMints = 
        map
          (\(ask,askSym) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askSym,1)]
              }
          )
          (zip newAskAssets newBeaconNames)

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy offerAsset
                  , Just (refScriptAddress, mintRefs!!0)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap $
                  askAsset : newAskAssets
              , mintTokens = mconcat
                  [ [(assetBeacon,fromIntegral (-numberChanged))]
                  , zip newBeaconNames $ repeat 1
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
          [
            UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

-- | Compse the `CreateSwap` and `CloseOrUpdate` redeemer to change what trading pair multiple
-- Swap UTxOs are for. The Swap UTxOs start as different offer assets and different ask assets
-- and are changed to other different offer assets and different ask assets.
benchTest17 :: Int -> EmulatorTrace ()
benchTest17 numberUpdated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let sellerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
      askAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [41..80]
      offerAssets = 
          map (\i -> (fst $ testToken1, fromString $ "TestToken" <> show @Int i)) [1..40]
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

  ( mintRefs,spendRef ) <- initializeScripts offerAssets

  let startSampleOutputs = 
        map 
          (\(ask,askSym,offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      , beaconName = askSym
                                      , askId = fst ask
                                      , askName = snd ask
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askSym 1
            <> uncurry singleton offer 10
            ))
          (zip4 (take 20 askAssets) 
                (take 20 askBeacons) 
                (take 20 offerAssets) 
                (take 20 beaconSyms))

  let sampleMints = 
        map
          (\(i,ask,askSym,offer) ->
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy offer
                  , Just (refScriptAddress, mintRefs!!i)
                  )
              , mintRedeemer = toRedeemer $ CreateSwap [ask]
              , mintTokens = [(askSym,1)]
              }
          )
          (zip4 [0..] 
                (take 20 askAssets) 
                (take 20 askBeacons) 
                (take 20 offerAssets))

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 startSampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = take 10 $ drop 10 sampleMints
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = swapAddr
              , outputUtxos = take 10 $ drop 10 startSampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  swaps <- take numberUpdated <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddr 

  let endSampleOutputs = 
        take numberUpdated $ map 
          (\(ask,askSym,offer,beaconSym) ->
            ( Just $ TxOutDatumInline
                   $ toDatum swapDatum{ offerId = fst offer
                                      , offerName = snd offer
                                      , beaconId = beaconSym
                                      , beaconName = askSym
                                      , askId = fst ask
                                      , askName = snd ask
                                      }
            , lovelaceValueOf 3_000_000
            <> singleton beaconSym askSym 1
            <> uncurry singleton offer 10
            ))
          (zip4 (take 20 $ drop 20 askAssets) 
                (take 20 $ drop 20 askBeacons) 
                (take 20 $ drop 20 offerAssets) 
                (take 20 $ drop 20 beaconSyms))

  let sampleBurns = 
        map
          (\(_,Just d) ->
            let oldOffer = (offerId d,offerName d)
                Just oldI = elemIndex oldOffer offerAssets
            in 
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy oldOffer
                    , Just (refScriptAddress, mintRefs!!oldI)
                    )
                , mintRedeemer = toRedeemer BurnBeacons 
                , mintTokens = [(beaconName d,-1)]
                }
          )
          swaps

  let endSampleMints = 
        take numberUpdated $ map
          (\(ask,askSym,offer) ->
            let Just i = elemIndex offer offerAssets
            in
              TokenMint 
                { mintWitness = 
                    ( beaconMintingPolicy offer
                    , Just (refScriptAddress, mintRefs!!i)
                    )
                , mintRedeemer = toRedeemer $ CreateSwap [ask]
                , mintTokens = [(askSym,1)]
                }
          )
          (zip3 (take 20 $ drop 20 askAssets) 
                (take 20 $ drop 20 askBeacons) 
                (take 20 $ drop 20 offerAssets))

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = sampleBurns <> endSampleMints
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
              , outputUtxos = endSampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest1

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
    , checkPredicateOptions opts "regressionTest10"
        assertNoFailedTransactions regressionTest10
    , checkPredicateOptions opts "regressionTest11"
        assertNoFailedTransactions regressionTest11
    , checkPredicateOptions opts "regressionTest12"
        assertNoFailedTransactions regressionTest12
    , checkPredicateOptions opts "regressionTest13"
        assertNoFailedTransactions regressionTest13
    , checkPredicateOptions opts "regressionTest14"
        assertNoFailedTransactions regressionTest14
    , checkPredicateOptions opts "regressionTest15"
        assertNoFailedTransactions regressionTest15
    , checkPredicateOptions opts "regressionTest16"
        assertNoFailedTransactions regressionTest16
    , checkPredicateOptions opts "regressionTest17"
        assertNoFailedTransactions regressionTest17
    , checkPredicateOptions opts "regressionTest18"
        assertNoFailedTransactions regressionTest18

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Swap price not > 0") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Swap price not > 0") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Swap price denominator not > 0") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Swap price denominator not > 0") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Beacons must be stored with some of the offered asset") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Beacons must be stored individually") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Invalid SwapDatum beacon_id") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Invalid SwapDatum beacon_name") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Invalid SwapDatum offer_id") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Invalid SwapDatum offer_name") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Invalid SwapDatum ask_id") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Invalid SwapDatum ask_name") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "No extraneous assets can be stored in the swap UTxO") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Invalid SwapDatum ask_name") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Invalid SwapDatum ask_name") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Beacons not stored at this swap address") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "Only the beacons in the redeemer can be minted/burned") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "Staking credential did not approve") failureTest24

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 55
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 7
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 56
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 8
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 54
    , checkPredicateOptions opts "benchTest6"
        assertNoFailedTransactions $ benchTest6 55
    , checkPredicateOptions opts "benchTest7"
        assertNoFailedTransactions $ benchTest7 14
    , checkPredicateOptions opts "benchTest8"
        assertNoFailedTransactions $ benchTest8 16
    , checkPredicateOptions opts "benchTest9"
        assertNoFailedTransactions $ benchTest9 38
    , checkPredicateOptions opts "benchTest10"
        assertNoFailedTransactions $ benchTest10 38
    , checkPredicateOptions opts "benchTest11"
        assertNoFailedTransactions $ benchTest11 16
    , checkPredicateOptions opts "benchTest12"
        assertNoFailedTransactions $ benchTest12 16
    , checkPredicateOptions opts "benchTest13"
        assertNoFailedTransactions $ benchTest13 38
    , checkPredicateOptions opts "benchTest14"
        assertNoFailedTransactions $ benchTest14 16
    , checkPredicateOptions opts "benchTest15"
        assertNoFailedTransactions $ benchTest15 14
    , checkPredicateOptions opts "benchTest16"
        assertNoFailedTransactions $ benchTest16 13
    , checkPredicateOptions opts "benchTest17"
        assertNoFailedTransactions $ benchTest17 10

      -- Performance Increases tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 56
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 8
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 57
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 9
    , checkPredicateOptions opts "perfIncreaseTest5"
        (Test.not assertNoFailedTransactions) $ benchTest5 55
    , checkPredicateOptions opts "perfIncreaseTest6"
        (Test.not assertNoFailedTransactions) $ benchTest6 56
    , checkPredicateOptions opts "perfIncreaseTest7"
        (Test.not assertNoFailedTransactions) $ benchTest7 15
    , checkPredicateOptions opts "perfIncreaseTest8"
        (Test.not assertNoFailedTransactions) $ benchTest8 17
    , checkPredicateOptions opts "perfIncreaseTest9"
        (Test.not assertNoFailedTransactions) $ benchTest9 39
    , checkPredicateOptions opts "perfIncreaseTest10"
        (Test.not assertNoFailedTransactions) $ benchTest10 39
    , checkPredicateOptions opts "perfIncreaseTest11"
        (Test.not assertNoFailedTransactions) $ benchTest11 17
    , checkPredicateOptions opts "perfIncreaseTest12"
        (Test.not assertNoFailedTransactions) $ benchTest12 17
    , checkPredicateOptions opts "perfIncreaseTest13"
        (Test.not assertNoFailedTransactions) $ benchTest13 39
    , checkPredicateOptions opts "perfIncreaseTest14"
        (Test.not assertNoFailedTransactions) $ benchTest14 17
    , checkPredicateOptions opts "perfIncreaseTest15"
        (Test.not assertNoFailedTransactions) $ benchTest15 15
    , checkPredicateOptions opts "perfIncreaseTest16"
        (Test.not assertNoFailedTransactions) $ benchTest16 14
    , checkPredicateOptions opts "perfIncreaseTest17"
        (Test.not assertNoFailedTransactions) $ benchTest17 11
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest24
