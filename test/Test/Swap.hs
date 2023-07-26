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
import Control.Monad (void,zipWithM_)
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
import Data.List (replicate)

import Test.Common
import CardanoSwaps

-------------------------------------------------
-- Initializations
------------------------------------------------
initializeRefScripts :: [DappScripts] -> Address -> EmulatorTrace ()
initializeRefScripts ds refAddr = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  -- | Create the spending reference script.
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript $ spendingValidator (ds!!0)
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitNSlots 2

  zipWithM_ 
    (\d n -> 
      (callEndpoint @"create-reference-script" h1 $
        CreateReferenceScriptParams
          { createReferenceScriptScript = unMintingPolicyScript $ beaconPolicy d
          , createReferenceScriptAddress = refAddr
          , createReferenceScriptUTxO = lovelaceValueOf (minUTxOMintRef + n)
          }
      )
      >> waitNSlots 2
    )
    ds
    [0..]

-------------------------------------------------
-- Swap Scenarios
-------------------------------------------------
successfullyExecuteSingleSwap :: [DappScripts] -> EmulatorTrace ()
successfullyExecuteSingleSwap ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

depositExtraneousAsset :: [DappScripts] -> EmulatorTrace ()
depositExtraneousAsset ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
             <> uncurry singleton testToken2 10
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

depositExtraneousADA :: [DappScripts] -> EmulatorTrace ()
depositExtraneousADA ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 22_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

inputMissingBeacon :: [DappScripts] -> EmulatorTrace ()
inputMissingBeacon ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = []
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

beaconWithdrawnFromAddress :: [DappScripts] -> EmulatorTrace ()
beaconWithdrawnFromAddress ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,2)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 20_000_001
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 20_000_001
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1,inputA2]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 30_000_001
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

notEnoughGiven :: [DappScripts] -> EmulatorTrace ()
notEnoughGiven ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 99
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

wrongPriceInDatumReturned :: [DappScripts] -> EmulatorTrace ()
wrongPriceInDatumReturned ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumDoesNotHaveWeightedAvg :: [DappScripts] -> EmulatorTrace ()
datumDoesNotHaveWeightedAvg ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,2)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum {swapPrice = unsafeRatio 20 1_000_000 }
            , lovelaceValueOf 20_000_000
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValueAndDatum 
              (lovelaceValueOf 20_000_000 <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1)
              adaTok1Datum
  inputA2 <- txOutRefWithValueAndDatum
              (lovelaceValueOf 20_000_000 <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1)
              adaTok1Datum {swapPrice = unsafeRatio 20 1_000_000}

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1,inputA2]
      , swapChange = 
          [ [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 10 1_000_000 }
              , lovelaceValueOf 30_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 2
             <> uncurry singleton testToken1 150
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumHasWrongBeaconId :: [DappScripts] -> EmulatorTrace ()
datumHasWrongBeaconId ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum {beaconId = ""}
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumHasWrongBeaconName :: [DappScripts] -> EmulatorTrace ()
datumHasWrongBeaconName ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum{beaconName = ""}
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumHasWrongOfferId :: [DappScripts] -> EmulatorTrace ()
datumHasWrongOfferId ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum{offerId = fst testToken1}
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumHasWrongOfferName :: [DappScripts] -> EmulatorTrace ()
datumHasWrongOfferName ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum {offerName = snd testToken1}
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumHasWrongAskId :: [DappScripts] -> EmulatorTrace ()
datumHasWrongAskId ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum {askId = ""}
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

datumHasWrongAskName :: [DappScripts] -> EmulatorTrace ()
datumHasWrongAskName ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = [swapAddr]
      , swapSpecificUTxOs = [inputA1]
      , swapChange = 
          [ [ ( Just adaTok1Datum{ askName = ""}
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

successfullyComposeSwaps :: [DappScripts] -> EmulatorTrace ()
successfullyComposeSwaps ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0
  tok1MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 1
  tok2MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 2

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      askTok3 = uncurry AssetConfig testToken3
      askTok3Name = genBeaconName askTok3
    
      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 1 1_000_000)

      tok1Tok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 1) 
          askTok2Name
          (fst testToken1)
          (snd testToken1)
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

      tok2Tok3Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 2) 
          askTok3Name
          (fst testToken2)
          (snd testToken2)
          (fst testToken3)
          (snd testToken3)
          (unsafeRatio 1 1)


  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)],[(askTok2Name,1)],[(askTok3Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1], MintBeacons [askTok2], MintBeacons [askTok3]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just tok1Tok2Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
           <> (uncurry singleton testToken1) 10
            )
          , ( Just tok2Tok3Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
           <> (uncurry singleton testToken2) 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef,tok1MintRef,tok2MintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  inputB1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                              <> (uncurry singleton testToken1) 10
  inputC1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
                              <> (uncurry singleton testToken2) 10

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = replicate 3 swapAddr
      , swapSpecificUTxOs = 
          [ inputA1
          , inputB1
          , inputC1
          ]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 10
              )
            ]
          , [ ( Just tok1Tok2Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
             <> uncurry singleton testToken2 10
              )
            ]
          , [ ( Just tok2Tok3Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
             <> uncurry singleton testToken3 10
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

mixUpBeaconsDuringComposition :: [DappScripts] -> EmulatorTrace ()
mixUpBeaconsDuringComposition ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0
  tok1MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 1
  tok2MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 2

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      askTok3 = uncurry AssetConfig testToken3
      askTok3Name = genBeaconName askTok3
    
      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 1 1_000_000)

      tok1Tok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 1) 
          askTok2Name
          (fst testToken1)
          (snd testToken1)
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

      tok2Tok3Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 2) 
          askTok3Name
          (fst testToken2)
          (snd testToken2)
          (fst testToken3)
          (snd testToken3)
          (unsafeRatio 1 1)


  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)],[(askTok2Name,1)],[(askTok3Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1], MintBeacons [askTok2], MintBeacons [askTok3]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just tok1Tok2Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
           <> (uncurry singleton testToken1) 10
            )
          , ( Just tok2Tok3Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
           <> (uncurry singleton testToken2) 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef,tok1MintRef,tok2MintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  inputB1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                              <> (uncurry singleton testToken1) 10
  inputC1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
                              <> (uncurry singleton testToken2) 10

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = replicate 3 swapAddr
      , swapSpecificUTxOs = 
          [ inputA1
          , inputB1
          , inputC1
          ]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 10
              )
            ]
          , [ ( Just tok1Tok2Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
             <> uncurry singleton testToken2 10
              )
            ]
          , [ ( Just tok2Tok3Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
             <> uncurry singleton testToken3 10
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

combineBeaconsDuringComposition :: [DappScripts] -> EmulatorTrace ()
combineBeaconsDuringComposition ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0
  tok1MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 1
  tok2MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 2

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      askTok3 = uncurry AssetConfig testToken3
      askTok3Name = genBeaconName askTok3
    
      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 1 1_000_000)

      tok1Tok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 1) 
          askTok2Name
          (fst testToken1)
          (snd testToken1)
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

      tok2Tok3Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 2) 
          askTok3Name
          (fst testToken2)
          (snd testToken2)
          (fst testToken3)
          (snd testToken3)
          (unsafeRatio 1 1)


  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1)],[(askTok2Name,1)],[(askTok3Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1], MintBeacons [askTok2], MintBeacons [askTok3]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just tok1Tok2Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
           <> (uncurry singleton testToken1) 10
            )
          , ( Just tok2Tok3Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
           <> (uncurry singleton testToken2) 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef,tok1MintRef,tok2MintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  inputB1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                              <> (uncurry singleton testToken1) 10
  inputC1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
                              <> (uncurry singleton testToken2) 10

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = replicate 2 swapAddr
      , swapSpecificUTxOs = 
          [ inputA1
          , inputB1
          , inputC1
          ]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 10
              )
            ]
          , [ ( Just tok1Tok2Datum
              , lovelaceValueOf 5_000_000
             <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
             <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
             <> uncurry singleton testToken3 10
             <> uncurry singleton testToken2 10
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

maxCompositionOfDifferentPairs :: [DappScripts] -> EmulatorTrace ()
maxCompositionOfDifferentPairs ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0
  tok1MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 1
  tok2MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 2
  tok3MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 3
  tok4MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 4
  tok5MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 5
  tok6MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 6
  tok7MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 7
  tok8MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 8
  tok9MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 9
  tok10MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 10
  tok11MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 11
  tok12MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 12
  tok13MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 13
  tok14MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 14

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      askTok3 = uncurry AssetConfig testToken3
      askTok3Name = genBeaconName askTok3

      askTok4 = uncurry AssetConfig testToken4
      askTok4Name = genBeaconName askTok4

      askTok5 = uncurry AssetConfig testToken5
      askTok5Name = genBeaconName askTok5

      askTok6 = uncurry AssetConfig testToken6
      askTok6Name = genBeaconName askTok6

      askTok7 = uncurry AssetConfig testToken7
      askTok7Name = genBeaconName askTok7

      askTok8 = uncurry AssetConfig testToken8
      askTok8Name = genBeaconName askTok8

      askTok9 = uncurry AssetConfig testToken9
      askTok9Name = genBeaconName askTok9

      askTok10 = uncurry AssetConfig testToken10
      askTok10Name = genBeaconName askTok10

      askTok11 = uncurry AssetConfig testToken11
      askTok11Name = genBeaconName askTok11

      askTok12 = uncurry AssetConfig testToken12
      askTok12Name = genBeaconName askTok12

      askTok13 = uncurry AssetConfig testToken13
      askTok13Name = genBeaconName askTok13

      askTok14 = uncurry AssetConfig testToken14
      askTok14Name = genBeaconName askTok14

      askTok15 = uncurry AssetConfig testToken15
      askTok15Name = genBeaconName askTok15

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 1 1_000_000)

      tok1Tok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 1) 
          askTok2Name
          (fst testToken1)
          (snd testToken1)
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

      tok2Tok3Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 2) 
          askTok3Name
          (fst testToken2)
          (snd testToken2)
          (fst testToken3)
          (snd testToken3)
          (unsafeRatio 1 1)

      tok3Tok4Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 3) 
          askTok4Name
          (fst testToken3)
          (snd testToken3)
          (fst testToken4)
          (snd testToken4)
          (unsafeRatio 1 1)
      
      tok4Tok5Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 4) 
          askTok5Name
          (fst testToken4)
          (snd testToken4)
          (fst testToken5)
          (snd testToken5)
          (unsafeRatio 1 1)

      tok5Tok6Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 5) 
          askTok6Name
          (fst testToken5)
          (snd testToken5)
          (fst testToken6)
          (snd testToken6)
          (unsafeRatio 1 1)

      tok6Tok7Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 6) 
          askTok7Name
          (fst testToken6)
          (snd testToken6)
          (fst testToken7)
          (snd testToken7)
          (unsafeRatio 1 1)
        
      tok7Tok8Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 7) 
          askTok8Name
          (fst testToken7)
          (snd testToken7)
          (fst testToken8)
          (snd testToken8)
          (unsafeRatio 1 1)
      
      tok8Tok9Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 8) 
          askTok9Name
          (fst testToken8)
          (snd testToken8)
          (fst testToken9)
          (snd testToken9)
          (unsafeRatio 1 1)

      tok9Tok10Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 9) 
          askTok10Name
          (fst testToken9)
          (snd testToken9)
          (fst testToken10)
          (snd testToken10)
          (unsafeRatio 1 1)
      
      tok10Tok11Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 10) 
          askTok11Name
          (fst testToken10)
          (snd testToken10)
          (fst testToken11)
          (snd testToken11)
          (unsafeRatio 1 1)

      tok11Tok12Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 11) 
          askTok12Name
          (fst testToken11)
          (snd testToken11)
          (fst testToken12)
          (snd testToken12)
          (unsafeRatio 1 1)

      tok12Tok13Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 12) 
          askTok13Name
          (fst testToken12)
          (snd testToken12)
          (fst testToken13)
          (snd testToken13)
          (unsafeRatio 1 1)


  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [ [(askTok1Name,1)]
          , [(askTok2Name,1)]
          , [(askTok3Name,1)]
          , [(askTok4Name,1)]
          , [(askTok5Name,1)]
          , [(askTok6Name,1)]
          , [(askTok7Name,1)]
          , [(askTok8Name,1)]
          , [(askTok9Name,1)]
          , [(askTok10Name,1)]
          , [(askTok11Name,1)]
          , [(askTok12Name,1)]
          , [(askTok13Name,1)]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons [askTok1]
          , MintBeacons [askTok2]
          , MintBeacons [askTok3]
          , MintBeacons [askTok4]
          , MintBeacons [askTok5]
          , MintBeacons [askTok6]
          , MintBeacons [askTok7]
          , MintBeacons [askTok8]
          , MintBeacons [askTok9]
          , MintBeacons [askTok10]
          , MintBeacons [askTok11]
          , MintBeacons [askTok12]
          , MintBeacons [askTok13]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just tok1Tok2Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
           <> (uncurry singleton testToken1) 10
            )
          , ( Just tok2Tok3Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
           <> (uncurry singleton testToken2) 10
            )
          , ( Just tok3Tok4Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!3) askTok4Name 1
           <> (uncurry singleton testToken3) 10
            )
          , ( Just tok4Tok5Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!4) askTok5Name 1
           <> (uncurry singleton testToken4) 10
            )
          , ( Just tok5Tok6Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!5) askTok6Name 1
           <> (uncurry singleton testToken5) 10
            )
          , ( Just tok6Tok7Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!6) askTok7Name 1
           <> (uncurry singleton testToken6) 10
            )
          , ( Just tok7Tok8Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!7) askTok8Name 1
           <> (uncurry singleton testToken7) 10
            )
          , ( Just tok8Tok9Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!8) askTok9Name 1
           <> (uncurry singleton testToken8) 10
            )
          , ( Just tok9Tok10Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!9) askTok10Name 1
           <> (uncurry singleton testToken9) 10
            )
          , ( Just tok10Tok11Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!10) askTok11Name 1
           <> (uncurry singleton testToken10) 10
            )
          , ( Just tok11Tok12Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!11) askTok12Name 1
           <> (uncurry singleton testToken11) 10
            )
          , ( Just tok12Tok13Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!12) askTok13Name 1
           <> (uncurry singleton testToken12) 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = 
          [ adaMintRef
          , tok1MintRef
          , tok2MintRef
          , tok3MintRef
          , tok4MintRef
          , tok5MintRef
          , tok6MintRef
          , tok7MintRef
          , tok8MintRef
          , tok9MintRef
          , tok10MintRef
          , tok11MintRef
          , tok12MintRef
          , tok13MintRef
          ]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  inputB1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                              <> (uncurry singleton testToken1) 10
  inputC1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
                              <> (uncurry singleton testToken2) 10
  inputD1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!3) askTok4Name 1
                              <> (uncurry singleton testToken3) 10
  inputE1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!4) askTok5Name 1
                              <> (uncurry singleton testToken4) 10
  inputF1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!5) askTok6Name 1
                              <> (uncurry singleton testToken5) 10
  inputG1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!6) askTok7Name 1
                              <> (uncurry singleton testToken6) 10
  inputH1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!7) askTok8Name 1
                              <> (uncurry singleton testToken7) 10
  inputI1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!8) askTok9Name 1
                              <> (uncurry singleton testToken8) 10
  inputJ1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!9) askTok10Name 1
                              <> (uncurry singleton testToken9) 10
  inputK1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!10) askTok11Name 1
                              <> (uncurry singleton testToken10) 10

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = replicate 11 swapAddr
      , swapSpecificUTxOs = 
          [ inputA1
          , inputB1
          , inputC1
          , inputD1
          , inputE1
          , inputF1
          , inputG1
          , inputH1
          , inputI1
          -- , inputJ1
          -- , inputK1
          ]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 10_000_000
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
             <> uncurry singleton testToken1 10
              )
            ]
          , [ ( Just tok1Tok2Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
             <> uncurry singleton testToken2 10
              )
            ]
          , [ ( Just tok2Tok3Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
             <> uncurry singleton testToken3 10
              )
            ]
          , [ ( Just tok3Tok4Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!3) askTok4Name 1
             <> uncurry singleton testToken4 10
              )
            ]
          , [ ( Just tok4Tok5Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!4) askTok5Name 1
             <> uncurry singleton testToken5 10
              )
            ]
          , [ ( Just tok5Tok6Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!5) askTok6Name 1
             <> uncurry singleton testToken6 10
              )
            ]
          , [ ( Just tok6Tok7Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!6) askTok7Name 1
             <> uncurry singleton testToken7 10
              )
            ]
          , [ ( Just tok7Tok8Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!7) askTok8Name 1
             <> uncurry singleton testToken8 10
              )
            ]
          , [ ( Just tok8Tok9Datum
              , lovelaceValueOf 2_500_000
             <> singleton (beaconCurrencySymbol $ ds!!8) askTok9Name 1
             <> uncurry singleton testToken9 10
              )
            ]
          -- , [ ( Just tok9Tok10Datum
          --     , lovelaceValueOf 2_500_000
          --    <> singleton (beaconCurrencySymbol $ ds!!9) askTok10Name 1
          --    <> uncurry singleton testToken10 10
          --     )
          --   ]
          -- , [ ( Just tok10Tok11Datum
          --     , lovelaceValueOf 2_500_000
          --    <> singleton (beaconCurrencySymbol $ ds!!10) askTok11Name 1
          --    <> uncurry singleton testToken10 10
          --     )
          --   ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

maxSwapAggregationsForSinglePair :: [DappScripts] -> EmulatorTrace ()
maxSwapAggregationsForSinglePair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )

  adaMintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 0

  let askTok1 = uncurry AssetConfig testToken1
      askTok1Name = genBeaconName askTok1

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,10)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 5_000_001
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_002
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_003
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_004
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_005
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_006
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_007
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_008
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_009
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_010
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  input1 <- txOutRefWithValue $ lovelaceValueOf 5_000_001
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input2 <- txOutRefWithValue $ lovelaceValueOf 5_000_002
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input3 <- txOutRefWithValue $ lovelaceValueOf 5_000_003
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input4 <- txOutRefWithValue $ lovelaceValueOf 5_000_004
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input5 <- txOutRefWithValue $ lovelaceValueOf 5_000_005
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input6 <- txOutRefWithValue $ lovelaceValueOf 5_000_006
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input7 <- txOutRefWithValue $ lovelaceValueOf 5_000_007
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input8 <- txOutRefWithValue $ lovelaceValueOf 5_000_008
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input9 <- txOutRefWithValue $ lovelaceValueOf 5_000_009
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input10 <- txOutRefWithValue $ lovelaceValueOf 5_000_010
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"swap" h2 $
    SwapParams
      { swapAddresses = replicate 11 swapAddr
      , swapSpecificUTxOs = 
          [ input1
          , input2
          , input3
          , input4
          , input5
          , input6
          -- , input7
          -- , input8
          -- , input9
          -- , input10
          ]
      , swapChange = 
          [ [ ( Just adaTok1Datum
              , lovelaceValueOf 20_000_021
             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 6
             <> uncurry singleton testToken1 100
              )
            ]
          ]
      , swapAsInline = True
      , swapScripts = ds!!0
      , swapWithRefScript = True
      , swapRefScript = spendRefScript
      , swapRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: [DappScripts] -> TestTree
tests ds = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Swap Assets"
    [ checkPredicateOptions opts "Successfully execute single swap"
        assertNoFailedTransactions (successfullyExecuteSingleSwap ds)
    , checkPredicateOptions opts "Fail if swap input missing swap beacon"
        (Test.not assertNoFailedTransactions) (inputMissingBeacon ds)
    , checkPredicateOptions opts "Fail if an extra beacon is withdrawn"
        (Test.not assertNoFailedTransactions) (beaconWithdrawnFromAddress ds)
    , checkPredicateOptions opts "Fail if not enough of the asked asset given"
        (Test.not assertNoFailedTransactions) (notEnoughGiven ds)
    , checkPredicateOptions opts "Fail if datum has wrong price (single swap)"
        (Test.not assertNoFailedTransactions) (wrongPriceInDatumReturned ds)
    , checkPredicateOptions opts "Fail if datum doesn't have weighted avg price of inputs"
        (Test.not assertNoFailedTransactions) (datumDoesNotHaveWeightedAvg ds)
    , checkPredicateOptions opts "Fail if beacon stored with datum with wrong beaconId"
        (Test.not assertNoFailedTransactions) (datumHasWrongBeaconId ds)
    , checkPredicateOptions opts "Fail if beacon stored with datum with wrong beaconName"
        (Test.not assertNoFailedTransactions) (datumHasWrongBeaconName ds)
    , checkPredicateOptions opts "Fail if beacon stored with datum with wrong offerId"
        (Test.not assertNoFailedTransactions) (datumHasWrongOfferId ds)
    , checkPredicateOptions opts "Fail if beacon stored with datum with wrong offerName"
        (Test.not assertNoFailedTransactions) (datumHasWrongOfferName ds)
    , checkPredicateOptions opts "Fail if beacon stored with datum with wrong askId"
        (Test.not assertNoFailedTransactions) (datumHasWrongAskId ds)
    , checkPredicateOptions opts "Fail if beacon stored with datum with wrong askName"
        (Test.not assertNoFailedTransactions) (datumHasWrongAskName ds)
    , checkPredicateOptions opts "Successfully compose swaps"
        assertNoFailedTransactions (successfullyComposeSwaps ds)
    , checkPredicateOptions opts "Fail if beacons mixed up in swap outputs"
        (Test.not assertNoFailedTransactions) (mixUpBeaconsDuringComposition ds)
    , checkPredicateOptions opts "Fail if beacons combined during swap composition"
        (Test.not assertNoFailedTransactions) (combineBeaconsDuringComposition ds)
    , checkPredicateOptions opts "Fail if extraneous asset deposited in swap output"
        (Test.not assertNoFailedTransactions) (depositExtraneousAsset ds)
    , checkPredicateOptions opts "Successfully deposit extra ADA"
        assertNoFailedTransactions (depositExtraneousADA ds)
    ]

testTrace :: [DappScripts] -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . maxCompositionOfDifferentPairs