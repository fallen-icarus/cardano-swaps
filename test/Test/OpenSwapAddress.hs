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

module Test.OpenSwapAddress
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)

import Test.Common
import CardanoSwaps

-------------------------------------------------
-- Open Swap Address Scenarios
-------------------------------------------------
successfullyOpenAddress :: TestScripts -> EmulatorTrace ()
successfullyOpenAddress ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

mintMultipleBeacons :: TestScripts -> EmulatorTrace ()
mintMultipleBeacons ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",2)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

wrongBeaconName :: TestScripts -> EmulatorTrace ()
wrongBeaconName ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("d",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "d" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

beaconGoesToWrongScriptAddress :: TestScripts -> EmulatorTrace ()
beaconGoesToWrongScriptAddress ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential alwaysSucceedValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

beaconMintedToNonStakingAddress :: TestScripts -> EmulatorTrace ()
beaconMintedToNonStakingAddress ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     Nothing

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

beaconMintedToPubkeyAddress :: TestScripts -> EmulatorTrace ()
beaconMintedToPubkeyAddress ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (PubKeyCredential $ unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 1)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

missingMinimumDeposit :: TestScripts -> EmulatorTrace ()
missingMinimumDeposit ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

wrongDatumSymbol :: TestScripts -> EmulatorTrace ()
wrongDatumSymbol ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol ""

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

wrongDatumType :: TestScripts -> EmulatorTrace ()
wrongDatumType ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just priceDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

mintWithBurnRedeemer :: TestScripts -> EmulatorTrace ()
mintWithBurnRedeemer ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = BurnBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

datumNotInline :: TestScripts -> EmulatorTrace ()
datumNotInline ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 10 1_000_000
      beaconDatum = BeaconSymbol beaconCurrencySymbol

      addr = Address (ScriptCredential spendingValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = False
      , openSwapAddressScripts = ts
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Open Swap Address"
    [ checkPredicateOptions opts "Successfully open new swap address"
        assertNoFailedTransactions (successfullyOpenAddress ts)
    , checkPredicateOptions opts "Fail if multiple beacons minted in tx"
        (Test.not assertNoFailedTransactions) (mintMultipleBeacons ts)
    , checkPredicateOptions opts "Fail if beacon has wrong token name"
        (Test.not assertNoFailedTransactions) (wrongBeaconName ts)
    , checkPredicateOptions opts "Fail if beacon is minted to non-dapp address"
        (Test.not assertNoFailedTransactions) (beaconGoesToWrongScriptAddress ts)
    , checkPredicateOptions opts "Fail if beacon is minted to address without staking"
        (Test.not assertNoFailedTransactions) (beaconMintedToNonStakingAddress ts)
    , checkPredicateOptions opts "Fail if beacon minted to pubkey address"
        (Test.not assertNoFailedTransactions) (beaconMintedToPubkeyAddress ts)
    , checkPredicateOptions opts "Fail if beacon not stored with minimum deposit"
        (Test.not assertNoFailedTransactions) (missingMinimumDeposit ts)
    , checkPredicateOptions opts "Fail if beacon not stored with proper beacon symbol"
        (Test.not assertNoFailedTransactions) (wrongDatumSymbol ts)
    , checkPredicateOptions opts "Fail if beacon stored with wrong datum type"
        (Test.not assertNoFailedTransactions) (wrongDatumType ts)
    , checkPredicateOptions opts "Fail if burn redeemer used to mint"
        (Test.not assertNoFailedTransactions) (mintWithBurnRedeemer ts)
    , checkPredicateOptions opts "Fail if output datum not inline"
        (Test.not assertNoFailedTransactions) (datumNotInline ts)
    ]

testTrace :: TestScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . mintWithBurnRedeemer