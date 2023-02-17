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

module Test.UpdateSwaps
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
-- Update Scenarios
-------------------------------------------------
updateAllUtxos :: EmulatorTrace ()
updateAllUtxos = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = True
      , updateSpecificUtxos = []
      , updatedOutputs =
          [ ( Just swapDatum'{swapBeacon'=Nothing}
            , lovelaceValueOf 100_000_000 <> singleton beaconSymbol' adaToken 1
            )
          ]
      , updateDatumsAsInline = True
      }

updateNonBeaconUtxos :: EmulatorTrace ()
updateNonBeaconUtxos = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = [(swapDatum', lovelaceValueOf 100_000_000)]
      , updatedOutputs =
          [ ( Just swapDatum'{swapBeacon'=Nothing}
            , lovelaceValueOf 50_000_000
            )
          ]
      , updateDatumsAsInline = True
      }

updateWithOtherBeaconInputs :: EmulatorTrace ()
updateWithOtherBeaconInputs = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = [(swapDatum', lovelaceValueOf 100_000_000)]
      , updatedOutputs =
          [ ( Just swapDatum'{swapBeacon'=Nothing}
            , lovelaceValueOf 50_000_000
            )
          , (Just swapDatum'{swapBeacon' = Nothing}
            , lovelaceValueOf 2_000_000 <> singleton beaconSymbol' adaToken 2
            )
          ]
      , updateDatumsAsInline = True
      }

updatePriceToNegativePrice :: EmulatorTrace ()
updatePriceToNegativePrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = [(swapDatum', lovelaceValueOf 100_000_000)]
      , updatedOutputs =
          [ ( Just SwapDatum'{swapPrice'=unsafeRatio (-1) 2, swapBeacon'=Nothing}
            , lovelaceValueOf 50_000_000
            )
          ]
      , updateDatumsAsInline = True
      }

updateDatumBeaconIdNotNothing :: EmulatorTrace ()
updateDatumBeaconIdNotNothing = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = [(swapDatum', lovelaceValueOf 100_000_000)]
      , updatedOutputs =
          [ ( Just swapDatum'
            , lovelaceValueOf 50_000_000
            )
          ]
      , updateDatumsAsInline = True
      }

updateDatumsNotInline :: EmulatorTrace ()
updateDatumsNotInline = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = [(swapDatum', lovelaceValueOf 100_000_000)]
      , updatedOutputs =
          [ ( Just swapDatum'{swapBeacon'=Nothing}
            , lovelaceValueOf 50_000_000
            )
          ]
      , updateDatumsAsInline = False
      }

updateAsNonOwner :: EmulatorTrace ()
updateAsNonOwner = do
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

  callEndpoint @"update-swaps" h2 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = [(swapDatum', lovelaceValueOf 100_000_000)]
      , updatedOutputs =
          [ ( Just swapDatum'{swapBeacon'=Nothing}
            , lovelaceValueOf 50_000_000
            )
          ]
      , updateDatumsAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Updating swaps"
    [ -- | No beacons allowed in inputs.
      checkPredicateOptions opts "Fail if swap's beacon utxo among inputs"
        (Test.not assertNoFailedTransactions) updateAllUtxos
    , checkPredicateOptions opts "Fail if any beacons among inputs"
        (Test.not assertNoFailedTransactions) updateWithOtherBeaconInputs
    , checkPredicateOptions opts "Successfully update non-beacon utxos"
        assertNoFailedTransactions updateNonBeaconUtxos
      
      -- | All outputs to address must contain proper datum.
    , checkPredicateOptions opts "Fail if new price is <= 0"
        (Test.not assertNoFailedTransactions) updatePriceToNegativePrice
    , checkPredicateOptions opts "Fail if new swapBeacon is not Nothing"
        (Test.not assertNoFailedTransactions) updateDatumBeaconIdNotNothing
    , checkPredicateOptions opts "Fail if new datums are not inline"
        (Test.not assertNoFailedTransactions) updateDatumsNotInline

      -- | Staking credential must approve.
    , checkPredicateOptions opts "Fail if stake pubkey did not sign"
        (Test.not assertNoFailedTransactions) updateAsNonOwner
    ]

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
benchmarkNonBeaconUpdates :: EmulatorTrace ()
benchmarkNonBeaconUpdates = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
            , lovelaceValueOf 10_000_001
            )
          , ( Just swapDatum'
            , lovelaceValueOf 10_000_002
            )
          , ( Just swapDatum'
            , lovelaceValueOf 10_000_003
            )
          , ( Just swapDatum'
            , lovelaceValueOf 10_000_004
            )
          , ( Just swapDatum'
            , lovelaceValueOf 10_000_005
            )
          , ( Just swapDatum'
            , lovelaceValueOf 10_000_006
            )
          , ( Just swapDatum'
            , lovelaceValueOf 10_000_007
            )
          ]
      , createLiveDatumsAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"update-swaps" h1 $
    UpdateSwapsParams
      { updateAddressSwapConfig = addressSwapConfig
      , updateAddress = swapAddress
      , updateAll = False
      , updateSpecificUtxos = 
          [ (swapDatum', lovelaceValueOf 10_000_001)
          , (swapDatum', lovelaceValueOf 10_000_002)
          , (swapDatum', lovelaceValueOf 10_000_003)
          , (swapDatum', lovelaceValueOf 10_000_004)
          , (swapDatum', lovelaceValueOf 10_000_005)
          , (swapDatum', lovelaceValueOf 10_000_006)
          , (swapDatum', lovelaceValueOf 10_000_007)
          ]
      , updatedOutputs =
          []
      , updateDatumsAsInline = True
      }

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def updateBenchConfig benchmarkNonBeaconUpdates