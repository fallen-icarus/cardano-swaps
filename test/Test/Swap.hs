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

module Test.Swap
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
-- Swap Scenarios
-------------------------------------------------
successfullySwap :: DappScripts -> EmulatorTrace ()
successfullySwap ts@DappScripts{..} = do
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

swapBeaconUTxO :: DappScripts -> EmulatorTrace ()
swapBeaconUTxO ts@DappScripts{..} = do
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( beaconDatum
            , lovelaceValueOf 20_000_000 <> singleton beaconCurrencySymbol "" 1
            )
          , ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 25_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

negativeInputPrice :: DappScripts -> EmulatorTrace ()
negativeInputPrice ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let priceDatum = SwapPrice $ unsafeRatio (-10) 1_000_000
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

zeroInputPrice :: DappScripts -> EmulatorTrace ()
zeroInputPrice ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 0 1_000_000
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

outputHasWrongDatumType :: DappScripts -> EmulatorTrace ()
outputHasWrongDatumType ts@DappScripts{..} = do
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just $ BeaconSymbol ""
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

singleSwapWrongOutputPrice :: DappScripts -> EmulatorTrace ()
singleSwapWrongOutputPrice ts@DappScripts{..} = do
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just $ SwapPrice $ unsafeRatio 5 1_000_000
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

successfullySwapMultipleUTxOs :: DappScripts -> EmulatorTrace ()
successfullySwapMultipleUTxOs ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let priceDatum1 = SwapPrice $ unsafeRatio 10 1_000_000
      priceDatum2 = SwapPrice $ unsafeRatio 20 1_000_000
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
          , ( Just priceDatum1
            , lovelaceValueOf 10_000_000
            )
          , ( Just priceDatum2
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      , openSwapAddressWithRefScript = False
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum1
            , lovelaceValueOf 10_000_000
            )
          , ( priceDatum2
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just $ SwapPrice $ unsafeRatio 15 1_000_000
            , lovelaceValueOf 10_000_000 <> (uncurry singleton testToken1) 150
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

outputPriceNotWeightedAvg :: DappScripts -> EmulatorTrace ()
outputPriceNotWeightedAvg ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let priceDatum1 = SwapPrice $ unsafeRatio 10 1_000_000
      priceDatum2 = SwapPrice $ unsafeRatio 20 1_000_000
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
          , ( Just priceDatum1
            , lovelaceValueOf 10_000_000
            )
          , ( Just priceDatum2
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      , openSwapAddressWithRefScript = False
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum1
            , lovelaceValueOf 10_000_000
            )
          , ( priceDatum2
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just $ SwapPrice $ unsafeRatio 10 1_000_000
            , lovelaceValueOf 10_000_000 <> (uncurry singleton testToken1) 150
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

requiredAmountDoesNotMatchWeightedPrice :: DappScripts -> EmulatorTrace ()
requiredAmountDoesNotMatchWeightedPrice ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let priceDatum1 = SwapPrice $ unsafeRatio 10 1_000_000
      priceDatum2 = SwapPrice $ unsafeRatio 20 1_000_000
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
          , ( Just priceDatum1
            , lovelaceValueOf 10_000_000
            )
          , ( Just priceDatum2
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      , openSwapAddressWithRefScript = False
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum1
            , lovelaceValueOf 10_000_000
            )
          , ( priceDatum2
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just $ SwapPrice $ unsafeRatio 15 1_000_000
            , lovelaceValueOf 10_000_000 <> (uncurry singleton testToken1) 149
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

singleSwapRatioDoesNotMatchInputPrice :: DappScripts -> EmulatorTrace ()
singleSwapRatioDoesNotMatchInputPrice ts@DappScripts{..} = do
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
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000
            )
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 49
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

otherAssetLeaves :: DappScripts -> EmulatorTrace ()
otherAssetLeaves ts@DappScripts{..} = do
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
            , lovelaceValueOf 10_000_000 <> (uncurry singleton testToken2) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      , openSwapAddressWithRefScript = False
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
          [ ( priceDatum
            , lovelaceValueOf 10_000_000 <> (uncurry singleton testToken2) 50
            )
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

benchSwap :: DappScripts -> EmulatorTrace ()
benchSwap ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let priceDatum = SwapPrice $ unsafeRatio 1 1_000_000
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
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts
      , openSwapAddressWithRefScript = False
      }

  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddress = addr
      , swapSpecificUtxos = 
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
          ]
      , swapChange =
          [ ( Just priceDatum
            , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 5
            )
          , ( Just priceDatum
            , lovelaceValueOf 5_000_001 <> (uncurry singleton testToken1) 5
            )
          , ( Just priceDatum
            , lovelaceValueOf 5_000_002 <> (uncurry singleton testToken1) 5
            )
          , ( Just priceDatum
            , lovelaceValueOf 5_000_003 <> (uncurry singleton testToken1) 5
            )
          , ( Just priceDatum
            , lovelaceValueOf 5_000_004 <> (uncurry singleton testToken1) 5
            )
          ]
      , swapChangeDatumAsInline = True
      , swapDappScripts = ts
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Swap Assets"
    [ checkPredicateOptions opts "Successfully swap"
        assertNoFailedTransactions (successfullySwap ts)
    , checkPredicateOptions opts "Fail if beacon UTxO in swap tx"
        (Test.not assertNoFailedTransactions) (swapBeaconUTxO ts)
    , checkPredicateOptions opts "Fail if swap input has negative price"
        (Test.not assertNoFailedTransactions) (negativeInputPrice ts)
    , checkPredicateOptions opts "Fail if swap input has zero price"
        (Test.not assertNoFailedTransactions) (zeroInputPrice ts)
    , checkPredicateOptions opts "Fail if swap output has BeaconSymbol datum"
        (Test.not assertNoFailedTransactions) (outputHasWrongDatumType ts)
    , checkPredicateOptions opts "Fail if single swap output has wrong price"
        (Test.not assertNoFailedTransactions) (singleSwapWrongOutputPrice ts)
    , checkPredicateOptions opts "Successfully swap multiple UTxOs"
        assertNoFailedTransactions (successfullySwapMultipleUTxOs ts)
    , checkPredicateOptions opts "Fail if output price /= weighted avg of inputs"
        (Test.not assertNoFailedTransactions) (outputPriceNotWeightedAvg ts)
    , checkPredicateOptions opts "Fail if swap ratio doesn't match weighted price"
        (Test.not assertNoFailedTransactions) (requiredAmountDoesNotMatchWeightedPrice ts)
    , checkPredicateOptions opts "Fail if single swap ratio doesn't match input price"
        (Test.not assertNoFailedTransactions) (singleSwapRatioDoesNotMatchInputPrice ts)
    , checkPredicateOptions opts "Fail if non-offer asset leaves address"
        (Test.not assertNoFailedTransactions) (otherAssetLeaves ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def benchConfig . benchSwap