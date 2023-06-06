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

module Test.Update
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import Control.Monad (void)
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
-- Update Swap Scenarios
-------------------------------------------------
successfullyUpdateSwap :: TestScripts -> EmulatorTrace ()
successfullyUpdateSwap ts@TestScripts{..} = do
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

  void $ waitUntilSlot 2

  callEndpoint @"update" h1 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateOutputs =
          [ ( Just $ SwapPrice $ unsafeRatio 5 1_000_000
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateAsInline = True
      , updateTestScripts = ts
      }

wrongInputDatumType :: TestScripts -> EmulatorTrace ()
wrongInputDatumType ts@TestScripts{..} = do
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

  void $ waitUntilSlot 2

  callEndpoint @"update" h1 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          , ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          ]
      , updateOutputs =
          [ ( Just $ SwapPrice $ unsafeRatio 5 1_000_000
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateAsInline = True
      , updateTestScripts = ts
      }

wrongOutputDatumType :: TestScripts -> EmulatorTrace ()
wrongOutputDatumType ts@TestScripts{..} = do
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

  void $ waitUntilSlot 2

  callEndpoint @"update" h1 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateOutputs =
          [ ( Just $ BeaconSymbol ""
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateAsInline = True
      , updateTestScripts = ts
      }

outputHasInvalidPrice :: TestScripts -> EmulatorTrace ()
outputHasInvalidPrice ts@TestScripts{..} = do
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

  void $ waitUntilSlot 2

  callEndpoint @"update" h1 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateOutputs =
          [ ( Just $ SwapPrice $ unsafeRatio (-5) 1_000_000
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateAsInline = True
      , updateTestScripts = ts
      }

stakingCredentialDidNotApprove :: TestScripts -> EmulatorTrace ()
stakingCredentialDidNotApprove ts@TestScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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

  void $ waitUntilSlot 2

  callEndpoint @"update" h2 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateOutputs =
          [ ( Just $ SwapPrice $ unsafeRatio 5 1_000_000
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateAsInline = True
      , updateTestScripts = ts
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
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

  void $ waitUntilSlot 2

  callEndpoint @"update" h1 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateOutputs =
          [ ( Just $ SwapPrice $ unsafeRatio 5 1_000_000
            , lovelaceValueOf 10_000_000
            )
          ]
      , updateAsInline = False
      , updateTestScripts = ts
      }

successfullyUpdateMultipleUTxOs :: TestScripts -> EmulatorTrace ()
successfullyUpdateMultipleUTxOs ts@TestScripts{..} = do
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
          , ( Just priceDatum
            , lovelaceValueOf 10_000_001
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_002
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_003
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_004
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_005
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_006
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_007
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_008
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_009
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_010
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_011
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_012
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_013
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_014
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_015
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_016
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_017
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_018
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_019
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      }

  void $ waitUntilSlot 2

  callEndpoint @"update" h1 $
    UpdateParams
      { updateSwapAddress = addr
      , updateSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_001
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_002
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_003
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_004
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_005
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_006
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_007
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_008
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_009
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_010
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_011
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_012
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_013
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_014
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_015
            )
          ]
      , updateOutputs =
          [ ( Just priceDatum
            , lovelaceValueOf 10_000_000
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_001
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_002
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_003
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_004
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_005
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_006
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_007
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_008
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_009
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_010
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_011
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_012
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_013
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_014
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_015
            )
          ]
      , updateAsInline = True
      , updateTestScripts = ts
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Swap Address"
    [ checkPredicateOptions opts "Successfully update a swap"
        assertNoFailedTransactions (successfullyUpdateSwap ts)
    , checkPredicateOptions opts "Fail if input has BeaconSymbol datum"
        (Test.not assertNoFailedTransactions) (wrongInputDatumType ts)
    , checkPredicateOptions opts "Fail if output has BeaconSymbol datum"
        (Test.not assertNoFailedTransactions) (wrongOutputDatumType ts)
    , checkPredicateOptions opts "Fail if output has negative price"
        (Test.not assertNoFailedTransactions) (outputHasInvalidPrice ts)
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) (stakingCredentialDidNotApprove ts)
    , checkPredicateOptions opts "Fail if output datum is not inline"
        (Test.not assertNoFailedTransactions) (datumNotInline ts)
    ]

testTrace :: TestScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . successfullyUpdateMultipleUTxOs