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

module Test.CloseSwapAddress
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

import Test.Common
import CardanoSwaps

-------------------------------------------------
-- Close Swap Address Scenarios
-------------------------------------------------
successfullyCloseAddress :: DappScripts -> EmulatorTrace ()
successfullyCloseAddress ts@DappScripts{..} = do
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
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-address" h1 $
    CloseAddressParams
      { closeBeaconsBurned = [("",-1)]
      , closeBeaconRedeemer = BurnBeacon
      , closeSwapAddress = addr
      , closeSpecificUtxos = 
          [ ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , closeDappScripts = ts
      }

beaconNotBurned :: DappScripts -> EmulatorTrace ()
beaconNotBurned ts@DappScripts{..} = do
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
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-address" h1 $
    CloseAddressParams
      { closeBeaconsBurned = []
      , closeBeaconRedeemer = BurnBeacon
      , closeSwapAddress = addr
      , closeSpecificUtxos = 
          [ ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , closeDappScripts = ts
      }

atLeastOneBeaconWithdrawn :: DappScripts -> EmulatorTrace ()
atLeastOneBeaconWithdrawn ts@DappScripts{..} = do
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
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 2

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addr
      , openSwapAddressInfo =
          [ ( Just beaconDatum
            , lovelaceValueOf 20_000_002 <> singleton beaconCurrencySymbol "" 1
            )
          , ( Just priceDatum
            , lovelaceValueOf 10_000_002
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-address" h1 $
    CloseAddressParams
      { closeBeaconsBurned = [("",-1)]
      , closeBeaconRedeemer = BurnBeacon
      , closeSwapAddress = addr
      , closeSpecificUtxos = 
          [ ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          , ( beaconDatum
            , lovelaceValueOf 20_000_002 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_002
            )
          ]
      , closeDappScripts = ts
      }

stakingCredentialDidNotApprove :: DappScripts -> EmulatorTrace ()
stakingCredentialDidNotApprove ts@DappScripts{..} = do
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
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-address" h2 $
    CloseAddressParams
      { closeBeaconsBurned = [("",-1)]
      , closeBeaconRedeemer = BurnBeacon
      , closeSwapAddress = addr
      , closeSpecificUtxos = 
          [ ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , closeDappScripts = ts
      }

successfullyCloseAddressWithMultipleUTxOs :: DappScripts -> EmulatorTrace ()
successfullyCloseAddressWithMultipleUTxOs ts@DappScripts{..} = do
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
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-address" h1 $
    CloseAddressParams
      { closeBeaconsBurned = [("",-1)]
      , closeBeaconRedeemer = BurnBeacon
      , closeSwapAddress = addr
      , closeSpecificUtxos = 
          [ ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
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
          , ( priceDatum
            , lovelaceValueOf 10_000_016
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_017
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_018
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_019
            )
          ]
      , closeDappScripts = ts
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Swap Address"
    [ checkPredicateOptions opts "Successfully close new swap address"
        assertNoFailedTransactions (successfullyCloseAddress ts)
    , checkPredicateOptions opts "Fail if address beacon withdrawn"
        (Test.not assertNoFailedTransactions) (beaconNotBurned ts)
    , checkPredicateOptions opts "Fail if at least one beacon withdrawn"
        (Test.not assertNoFailedTransactions) (atLeastOneBeaconWithdrawn ts)
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) (stakingCredentialDidNotApprove ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def benchConfig . successfullyCloseAddressWithMultipleUTxOs