{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.SwapAssets
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Monad (void)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address

import Test.Common

import CardanoSwaps (beaconSymbol,swapValidatorHash)

-------------------------------------------------
-- Swap Scenarios
-------------------------------------------------
swapSingleUtxo :: EmulatorTrace ()
swapSingleUtxo = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 100_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [(swapDatum', lovelaceValueOf 100_000_000)]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 2 1, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 20
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapSingleUtxoWithTooLittleGiven :: EmulatorTrace ()
swapSingleUtxoWithTooLittleGiven = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 100_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [(swapDatum', lovelaceValueOf 100_000_000)]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 2 1, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 19
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapAndTakeOtherAsset :: EmulatorTrace ()
swapAndTakeOtherAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 100_000_000 <> (uncurry singleton testToken2) 5
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [(swapDatum', lovelaceValueOf 100_000_000 <> (uncurry singleton testToken2) 5)]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 2 1, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 20
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapSingleUtxoWithWrongPrice :: EmulatorTrace ()
swapSingleUtxoWithWrongPrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 100_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [(swapDatum', lovelaceValueOf 100_000_000)]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 1 1, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 20
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapSingleUtxoWithWrongBeaconId :: EmulatorTrace ()
swapSingleUtxoWithWrongBeaconId = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 100_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [(swapDatum', lovelaceValueOf 100_000_000)]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 2 1, swapBeacon' = Just adaSymbol}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 20
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapMultipleUtxos :: EmulatorTrace ()
swapMultipleUtxos = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 50_000_000
            )
          , ( Just swapDatum'{swapPrice' = unsafeRatio 1 1}
            , lovelaceValueOf 50_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [ (swapDatum', lovelaceValueOf 50_000_000)
          , (swapDatum'{swapPrice' = unsafeRatio 1 1}, lovelaceValueOf 50_000_000)
          ]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 3 2, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 15
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapMultipleUtxosWithWrongPrice :: EmulatorTrace ()
swapMultipleUtxosWithWrongPrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 50_000_000
            )
          , ( Just swapDatum'{swapPrice' = unsafeRatio 1 1}
            , lovelaceValueOf 50_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [ (swapDatum', lovelaceValueOf 50_000_000)
          , (swapDatum'{swapPrice' = unsafeRatio 1 1}, lovelaceValueOf 50_000_000)
          ]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 1 2, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 15
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapMultipleUtxosWithNegativePriceInput :: EmulatorTrace ()
swapMultipleUtxosWithNegativePriceInput = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 50_000_000
            )
          , ( Just swapDatum'{swapPrice' = unsafeRatio (-1) 1}
            , lovelaceValueOf 50_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [ (swapDatum', lovelaceValueOf 50_000_000)
          , (swapDatum'{swapPrice' = unsafeRatio (-1) 1}, lovelaceValueOf 50_000_000)
          ]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 3 2, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 15
           )
         ]
      , swapChangeDatumAsInline = True
      }

swapSingleUtxoWithNonInlineDatum :: EmulatorTrace ()
swapSingleUtxoWithNonInlineDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let beaconSwapConfig = swapConfig1
      addressSwapConfig = swapConfig1

      beaconSymbol' = beaconSymbol $ convert2SwapConfig beaconSwapConfig

      swapDatum' = SwapDatum'
        { swapPrice' = unsafeRatio 2 1
        , swapBeacon' = Just beaconSymbol'
        }

      swapAddress = Address 
        (ScriptCredential $ swapValidatorHash $ convert2SwapConfig addressSwapConfig)
        (Just $ StakingHash 
              $ PubKeyCredential 
              $ unPaymentPubKeyHash 
              $ mockWalletPaymentPubKeyHash 
              $ knownWallet 1)

  callEndpoint @"create-live-swap-address" h1 $
    CreateLiveSwapAddressParams
      { beaconsMinted = [(adaToken,1)]
      , useMintRedeemer = True
      , createLiveBeaconSwapConfig = beaconSwapConfig
      , createLiveAddressSwapConfig = addressSwapConfig
      , createLiveAddress = swapAddress
      , createLiveRefScript = Proper
      , createLiveRefScriptUtxo =
          ( Just swapDatum'
          , singleton beaconSymbol' adaToken 1 <> refScriptDeposit
          )
      , createLiveInitialPositions =
          [ ( Just swapDatum'
            , lovelaceValueOf 100_000_000
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap-assets" h2 $
    SwapAssetsParams
      { swapAddressSwapConfig = addressSwapConfig
      , swappableAddress = swapAddress
      , swapUtxos =
          [(swapDatum', lovelaceValueOf 100_000_000)]
      , swapChange =
         [ ( Just SwapDatum'{swapPrice' = unsafeRatio 2 1, swapBeacon' = Nothing}
           , lovelaceValueOf 90_000_000 <> (uncurry singleton testToken1) 20
           )
         ]
      , swapChangeDatumAsInline = False
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Swapping assets"
    [ checkPredicateOptions opts "Succeed with single utxo swap"
        assertNoFailedTransactions swapSingleUtxo
    , checkPredicateOptions opts "Fail if too little of asked asset given for swap"
        (Test.not assertNoFailedTransactions) swapSingleUtxoWithTooLittleGiven
    , checkPredicateOptions opts "Fail if more than just the offered asset taken"
        (Test.not assertNoFailedTransactions) swapAndTakeOtherAsset
    , checkPredicateOptions opts "Fail if swap output does not contain same price (single utxo swap)"
        (Test.not assertNoFailedTransactions) swapSingleUtxoWithWrongPrice
    , checkPredicateOptions opts "Fail if swap output datum does not have Nothing for swapBeacon"
        (Test.not assertNoFailedTransactions) swapSingleUtxoWithWrongBeaconId
    , checkPredicateOptions opts "Successfully swap multiple utxos"
        assertNoFailedTransactions swapMultipleUtxos
    , checkPredicateOptions opts "Fail if swap output datum does not contain weighted avg price (multiple utxo swap)"
        (Test.not assertNoFailedTransactions) swapMultipleUtxosWithWrongPrice
    , checkPredicateOptions opts "Fail if a swap input has a negative price"
        (Test.not assertNoFailedTransactions) swapMultipleUtxosWithNegativePriceInput
    , checkPredicateOptions opts "Fail if swap output datum is not inline"
        (Test.not assertNoFailedTransactions) swapSingleUtxoWithNonInlineDatum
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig swapSingleUtxoWithNonInlineDatum