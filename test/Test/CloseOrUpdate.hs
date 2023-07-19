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

module Test.CloseOrUpdate
(
  tests,
  testTrace
) where

import Prelude (IO,drop)
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
-- Close/Update Scenarios
-------------------------------------------------
successfullyCloseSingleSwap :: [DappScripts] -> EmulatorTrace ()
successfullyCloseSingleSwap ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-1)]]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyUpdateSingleSwap :: [DappScripts] -> EmulatorTrace ()
successfullyUpdateSingleSwap ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{swapPrice = unsafeRatio 1 1_000_000}
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyCloseMultipleSwapsOfSamePair :: [DappScripts] -> EmulatorTrace ()
successfullyCloseMultipleSwapsOfSamePair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
            , lovelaceValueOf 15_000_000 
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-2)]]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyUpdateMultipleSwapsOfSamePair :: [DappScripts] -> EmulatorTrace ()
successfullyUpdateMultipleSwapsOfSamePair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
            , lovelaceValueOf 15_000_000 
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyMixUpdateAndCloseOfSamePair :: [DappScripts] -> EmulatorTrace ()
successfullyMixUpdateAndCloseOfSamePair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
            , lovelaceValueOf 15_000_000 
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-1)]]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyMixUpdateAndCloseOfDifferentPairs :: [DappScripts] -> EmulatorTrace ()
successfullyMixUpdateAndCloseOfDifferentPairs ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1),(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1,askTok2]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok2Name,-1)]]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

onlyBeaconWithdrawnDuringClose :: [DappScripts] -> EmulatorTrace ()
onlyBeaconWithdrawnDuringClose ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

onlyBeaconWithdrawnDuringUpdate :: [DappScripts] -> EmulatorTrace ()
onlyBeaconWithdrawnDuringUpdate ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{swapPrice = unsafeRatio 1 1_000_000}
            , lovelaceValueOf 20_000_000 
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

updateHasNegativePrice :: [DappScripts] -> EmulatorTrace ()
updateHasNegativePrice ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{swapPrice = unsafeRatio (-1) 1_000_000}
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

updateHasZeroPrice :: [DappScripts] -> EmulatorTrace ()
updateHasZeroPrice ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{swapPrice = unsafeRatio 0 1_000_000}
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

ownerDidNotApprove :: [DappScripts] -> EmulatorTrace ()
ownerDidNotApprove ds = do
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

  callEndpoint @"close-or-update" h2 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-1)]]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

updateNotStoredWithOfferAsset :: [DappScripts] -> EmulatorTrace ()
updateNotStoredWithOfferAsset ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         (Just $ StakingHash
                               $ PubKeyCredential
                               $ unPaymentPubKeyHash
                               $ mockWalletPaymentPubKeyHash
                               $ knownWallet 1
                         )
  
  tok1MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 1

  let askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      tok1Tok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 1) 
          askTok2Name
          (fst testToken1)
          (snd testToken1)
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok2]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just tok1Tok2Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
           <> uncurry singleton testToken1 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = tail ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [tok1MintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  inputA1 <- txOutRefWithValue $ lovelaceValueOf 2_500_000 
                              <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                              <> uncurry singleton testToken1 10

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just tok1Tok2Datum{swapPrice = unsafeRatio 1 2}
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = tail ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [tok1MintRef]
      , closeOrUpdateRefAddress = refAddr
      }

beaconsGroupedUpForSamePair :: [DappScripts] -> EmulatorTrace ()
beaconsGroupedUpForSamePair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
            , lovelaceValueOf 15_000_000 
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 2
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

beaconsGroupedUpForDifferentPairs :: [DappScripts] -> EmulatorTrace ()
beaconsGroupedUpForDifferentPairs ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1),(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1,askTok2]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

beaconsMixedUpDuringUpdate :: [DappScripts] -> EmulatorTrace ()
beaconsMixedUpDuringUpdate ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 10 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1),(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1,askTok2]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
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
  inputA2 <- txOutRefWithValue $ lovelaceValueOf 15_000_000 
                              <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ inputA1
          , inputA2
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyComposeMintAndCloseOrUpdateWithIndependentUTxOs :: [DappScripts] -> EmulatorTrace ()
successfullyComposeMintAndCloseOrUpdateWithIndependentUTxOs ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)
      
      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok2Name,1)]]
      , closeOrUpdateBeaconRedeemer = MintBeacons [askTok2]
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

successfullyComposeMintAndCloseOrUpdateWithDependentUTxOs :: [DappScripts] -> EmulatorTrace ()
successfullyComposeMintAndCloseOrUpdateWithDependentUTxOs ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)
      
      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-1),(askTok2Name,1)]]
      , closeOrUpdateBeaconRedeemer = MintBeacons [askTok1,askTok2]
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

burnedBeaconNotPresentInRedeemer :: [DappScripts] -> EmulatorTrace ()
burnedBeaconNotPresentInRedeemer ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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

      askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)
      
      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
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

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-1),(askTok2Name,1)]]
      , closeOrUpdateBeaconRedeemer = MintBeacons [askTok2]
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = [inputA1]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok2Datum
            , lovelaceValueOf 15_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

maxCloseForSameTradingPair :: [DappScripts] -> EmulatorTrace ()
maxCloseForSameTradingPair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
      { createSwapBeaconsMinted = [[(askTok1Name,50)]]
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
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_011
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_012
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_013
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_014
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_015
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_016
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_017
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_018
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_019
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_020
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_021
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_022
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_023
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_024
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_025
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_026
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_027
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_028
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_029
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_030
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_031
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_032
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_033
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_034
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_035
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_036
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_037
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_038
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_039
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_040
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_041
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_042
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_043
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_044
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_045
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_046
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_047
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_048
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_049
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_050
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
  input11 <- txOutRefWithValue $ lovelaceValueOf 5_000_011
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input12 <- txOutRefWithValue $ lovelaceValueOf 5_000_012
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input13 <- txOutRefWithValue $ lovelaceValueOf 5_000_013
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input14 <- txOutRefWithValue $ lovelaceValueOf 5_000_014
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input15 <- txOutRefWithValue $ lovelaceValueOf 5_000_015
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input16 <- txOutRefWithValue $ lovelaceValueOf 5_000_016
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input17 <- txOutRefWithValue $ lovelaceValueOf 5_000_017
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input18 <- txOutRefWithValue $ lovelaceValueOf 5_000_018
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input19 <- txOutRefWithValue $ lovelaceValueOf 5_000_019
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input20 <- txOutRefWithValue $ lovelaceValueOf 5_000_020
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input21 <- txOutRefWithValue $ lovelaceValueOf 5_000_021
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input22 <- txOutRefWithValue $ lovelaceValueOf 5_000_022
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input23 <- txOutRefWithValue $ lovelaceValueOf 5_000_023
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input24 <- txOutRefWithValue $ lovelaceValueOf 5_000_024
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input25 <- txOutRefWithValue $ lovelaceValueOf 5_000_025
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input26 <- txOutRefWithValue $ lovelaceValueOf 5_000_026
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input27 <- txOutRefWithValue $ lovelaceValueOf 5_000_027
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input28 <- txOutRefWithValue $ lovelaceValueOf 5_000_028
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input29 <- txOutRefWithValue $ lovelaceValueOf 5_000_029
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input30 <- txOutRefWithValue $ lovelaceValueOf 5_000_030
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input31 <- txOutRefWithValue $ lovelaceValueOf 5_000_031
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input32 <- txOutRefWithValue $ lovelaceValueOf 5_000_032
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input33 <- txOutRefWithValue $ lovelaceValueOf 5_000_033
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input34 <- txOutRefWithValue $ lovelaceValueOf 5_000_034
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input35 <- txOutRefWithValue $ lovelaceValueOf 5_000_035
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input36 <- txOutRefWithValue $ lovelaceValueOf 5_000_036
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input37 <- txOutRefWithValue $ lovelaceValueOf 5_000_037
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input38 <- txOutRefWithValue $ lovelaceValueOf 5_000_038
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input39 <- txOutRefWithValue $ lovelaceValueOf 5_000_039
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input40 <- txOutRefWithValue $ lovelaceValueOf 5_000_040
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input41 <- txOutRefWithValue $ lovelaceValueOf 5_000_041
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input42 <- txOutRefWithValue $ lovelaceValueOf 5_000_042
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input43 <- txOutRefWithValue $ lovelaceValueOf 5_000_043
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input44 <- txOutRefWithValue $ lovelaceValueOf 5_000_044
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input45 <- txOutRefWithValue $ lovelaceValueOf 5_000_045
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input46 <- txOutRefWithValue $ lovelaceValueOf 5_000_046
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input47 <- txOutRefWithValue $ lovelaceValueOf 5_000_047
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input48 <- txOutRefWithValue $ lovelaceValueOf 5_000_048
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input49 <- txOutRefWithValue $ lovelaceValueOf 5_000_049
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input50 <- txOutRefWithValue $ lovelaceValueOf 5_000_050
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = [[(askTok1Name,-47)]]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ input1
          , input2
          , input3
          , input4
          , input5
          , input6
          , input7
          , input8
          , input9
          , input10
          , input11
          , input12
          , input13
          , input14
          , input15
          , input16
          , input17
          , input18
          , input19
          , input20
          , input21
          , input22
          , input23
          , input24
          , input25
          , input26
          , input27
          , input28
          , input29
          , input30
          , input31
          , input32
          , input33
          , input34
          , input35
          , input36
          , input37
          , input38
          , input39
          , input40
          , input41
          , input42
          , input43
          , input44
          , input45
          , input46
          , input47
          -- , input48
          -- , input49
          -- , input50
          ]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

maxCloseForSameOfferButDifferentAsks :: [DappScripts] -> EmulatorTrace ()
maxCloseForSameOfferButDifferentAsks ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

      askTok16 = uncurry AssetConfig testToken16
      askTok16Name = genBeaconName askTok16

      askTok17 = uncurry AssetConfig testToken17
      askTok17Name = genBeaconName askTok17

      askTok18 = uncurry AssetConfig testToken18
      askTok18Name = genBeaconName askTok18

      askTok19 = uncurry AssetConfig testToken19
      askTok19Name = genBeaconName askTok19

      askTok20 = uncurry AssetConfig testToken20
      askTok20Name = genBeaconName askTok20

      askTok21 = uncurry AssetConfig testToken21
      askTok21Name = genBeaconName askTok21

      askTok22 = uncurry AssetConfig testToken22
      askTok22Name = genBeaconName askTok22

      askTok23 = uncurry AssetConfig testToken23
      askTok23Name = genBeaconName askTok23

      askTok24 = uncurry AssetConfig testToken24
      askTok24Name = genBeaconName askTok24

      askTok25 = uncurry AssetConfig testToken25
      askTok25Name = genBeaconName askTok25

      askTok26 = uncurry AssetConfig testToken26
      askTok26Name = genBeaconName askTok26

      askTok27 = uncurry AssetConfig testToken27
      askTok27Name = genBeaconName askTok27

      askTok28 = uncurry AssetConfig testToken28
      askTok28Name = genBeaconName askTok28

      askTok29 = uncurry AssetConfig testToken29
      askTok29Name = genBeaconName askTok29

      askTok30 = uncurry AssetConfig testToken30
      askTok30Name = genBeaconName askTok30

      askTok31 = uncurry AssetConfig testToken31
      askTok31Name = genBeaconName askTok31

      askTok32 = uncurry AssetConfig testToken32
      askTok32Name = genBeaconName askTok32

      askTok33 = uncurry AssetConfig testToken33
      askTok33Name = genBeaconName askTok33

      askTok34 = uncurry AssetConfig testToken34
      askTok34Name = genBeaconName askTok34

      askTok35 = uncurry AssetConfig testToken35
      askTok35Name = genBeaconName askTok35

      askTok36 = uncurry AssetConfig testToken36
      askTok36Name = genBeaconName askTok36

      askTok37 = uncurry AssetConfig testToken37
      askTok37Name = genBeaconName askTok37

      askTok38 = uncurry AssetConfig testToken38
      askTok38Name = genBeaconName askTok38

      askTok39 = uncurry AssetConfig testToken39
      askTok39Name = genBeaconName askTok39

      askTok40 = uncurry AssetConfig testToken40
      askTok40Name = genBeaconName askTok40

      askTok41 = uncurry AssetConfig testToken41
      askTok41Name = genBeaconName askTok41

      askTok42 = uncurry AssetConfig testToken42
      askTok42Name = genBeaconName askTok42

      askTok43 = uncurry AssetConfig testToken43
      askTok43Name = genBeaconName askTok43

      askTok44 = uncurry AssetConfig testToken44
      askTok44Name = genBeaconName askTok44

      askTok45 = uncurry AssetConfig testToken45
      askTok45Name = genBeaconName askTok45

      askTok46 = uncurry AssetConfig testToken46
      askTok46Name = genBeaconName askTok46

      askTok47 = uncurry AssetConfig testToken47
      askTok47Name = genBeaconName askTok47

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

      adaTok3Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok3Name
          ""
          ""
          (fst testToken3)
          (snd testToken3)
          (unsafeRatio 1 1)

      adaTok4Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok4Name
          ""
          ""
          (fst testToken4)
          (snd testToken4)
          (unsafeRatio 1 1)
      
      adaTok5Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok5Name
          ""
          ""
          (fst testToken5)
          (snd testToken5)
          (unsafeRatio 1 1)

      adaTok6Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok6Name
          ""
          ""
          (fst testToken6)
          (snd testToken6)
          (unsafeRatio 1 1)

      adaTok7Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok7Name
          ""
          ""
          (fst testToken7)
          (snd testToken7)
          (unsafeRatio 1 1)
        
      adaTok8Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok8Name
          ""
          ""
          (fst testToken8)
          (snd testToken8)
          (unsafeRatio 1 1)
      
      adaTok9Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok9Name
          ""
          ""
          (fst testToken9)
          (snd testToken9)
          (unsafeRatio 1 1)

      adaTok10Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok10Name
          ""
          ""
          (fst testToken10)
          (snd testToken10)
          (unsafeRatio 1 1)
      
      adaTok11Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok11Name
          ""
          ""
          (fst testToken11)
          (snd testToken11)
          (unsafeRatio 1 1)

      adaTok12Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok12Name
          ""
          ""
          (fst testToken12)
          (snd testToken12)
          (unsafeRatio 1 1)

      adaTok13Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok13Name
          ""
          ""
          (fst testToken13)
          (snd testToken13)
          (unsafeRatio 1 1)

      adaTok14Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok14Name
          ""
          ""
          (fst testToken14)
          (snd testToken14)
          (unsafeRatio 1 1)

      adaTok15Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok15Name
          ""
          ""
          (fst testToken15)
          (snd testToken15)
          (unsafeRatio 1 1)

      adaTok16Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok16Name
          ""
          ""
          (fst testToken16)
          (snd testToken16)
          (unsafeRatio 1 1)

      adaTok17Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok17Name
          ""
          ""
          (fst testToken17)
          (snd testToken17)
          (unsafeRatio 1 1)
      
      adaTok18Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok18Name
          ""
          ""
          (fst testToken18)
          (snd testToken18)
          (unsafeRatio 1 1)

      adaTok19Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok19Name
          ""
          ""
          (fst testToken19)
          (snd testToken19)
          (unsafeRatio 1 1)

      adaTok20Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok20Name
          ""
          ""
          (fst testToken20)
          (snd testToken20)
          (unsafeRatio 1 1)

      adaTok21Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok21Name
          ""
          ""
          (fst testToken21)
          (snd testToken21)
          (unsafeRatio 10 1_000_000)

      adaTok22Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok22Name
          ""
          ""
          (fst testToken22)
          (snd testToken22)
          (unsafeRatio 1 1)

      adaTok23Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok23Name
          ""
          ""
          (fst testToken23)
          (snd testToken23)
          (unsafeRatio 1 1)

      adaTok24Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok24Name
          ""
          ""
          (fst testToken24)
          (snd testToken24)
          (unsafeRatio 1 1)
      
      adaTok25Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok25Name
          ""
          ""
          (fst testToken25)
          (snd testToken25)
          (unsafeRatio 1 1)

      adaTok26Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok26Name
          ""
          ""
          (fst testToken26)
          (snd testToken26)
          (unsafeRatio 1 1)

      adaTok27Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok27Name
          ""
          ""
          (fst testToken27)
          (snd testToken27)
          (unsafeRatio 1 1)
        
      adaTok28Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok28Name
          ""
          ""
          (fst testToken28)
          (snd testToken28)
          (unsafeRatio 1 1)
      
      adaTok29Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok29Name
          ""
          ""
          (fst testToken29)
          (snd testToken29)
          (unsafeRatio 1 1)

      adaTok30Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok30Name
          ""
          ""
          (fst testToken30)
          (snd testToken30)
          (unsafeRatio 1 1)
      
      adaTok31Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok31Name
          ""
          ""
          (fst testToken31)
          (snd testToken31)
          (unsafeRatio 1 1)

      adaTok32Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok32Name
          ""
          ""
          (fst testToken32)
          (snd testToken32)
          (unsafeRatio 1 1)

      adaTok33Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok33Name
          ""
          ""
          (fst testToken33)
          (snd testToken33)
          (unsafeRatio 1 1)

      adaTok34Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok34Name
          ""
          ""
          (fst testToken34)
          (snd testToken34)
          (unsafeRatio 1 1)

      adaTok35Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok35Name
          ""
          ""
          (fst testToken35)
          (snd testToken35)
          (unsafeRatio 1 1)

      adaTok36Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok36Name
          ""
          ""
          (fst testToken36)
          (snd testToken36)
          (unsafeRatio 1 1)

      adaTok37Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok37Name
          ""
          ""
          (fst testToken37)
          (snd testToken37)
          (unsafeRatio 1 1)
      
      adaTok38Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok38Name
          ""
          ""
          (fst testToken38)
          (snd testToken38)
          (unsafeRatio 1 1)

      adaTok39Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok39Name
          ""
          ""
          (fst testToken39)
          (snd testToken39)
          (unsafeRatio 1 1)

      adaTok40Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok40Name
          ""
          ""
          (fst testToken40)
          (snd testToken40)
          (unsafeRatio 1 1)

      adaTok41Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok41Name
          ""
          ""
          (fst testToken41)
          (snd testToken41)
          (unsafeRatio 1 1)

      adaTok42Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok42Name
          ""
          ""
          (fst testToken42)
          (snd testToken42)
          (unsafeRatio 1 1)

      adaTok43Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok43Name
          ""
          ""
          (fst testToken43)
          (snd testToken43)
          (unsafeRatio 1 1)

      adaTok44Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok44Name
          ""
          ""
          (fst testToken44)
          (snd testToken44)
          (unsafeRatio 1 1)

      adaTok45Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok45Name
          ""
          ""
          (fst testToken45)
          (snd testToken45)
          (unsafeRatio 1 1)

      adaTok46Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok46Name
          ""
          ""
          (fst testToken46)
          (snd testToken46)
          (unsafeRatio 1 1)

      adaTok47Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok47Name
          ""
          ""
          (fst testToken47)
          (snd testToken47)
          (unsafeRatio 1 1)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [
            [ (askTok1Name,1)
            , (askTok2Name,1)
            , (askTok3Name,1)
            , (askTok4Name,1)
            , (askTok5Name,1)
            , (askTok6Name,1)
            , (askTok7Name,1)
            , (askTok8Name,1)
            , (askTok9Name,1)
            , (askTok10Name,1)
            , (askTok11Name,1)
            , (askTok12Name,1)
            , (askTok13Name,1)
            , (askTok14Name,1)
            , (askTok15Name,1)
            , (askTok16Name,1)
            , (askTok17Name,1)
            , (askTok18Name,1)
            , (askTok19Name,1)
            , (askTok20Name,1)
            , (askTok21Name,1)
            , (askTok22Name,1)
            , (askTok23Name,1)
            , (askTok24Name,1)
            , (askTok25Name,1)
            , (askTok26Name,1)
            , (askTok27Name,1)
            , (askTok28Name,1)
            , (askTok29Name,1)
            , (askTok30Name,1)
            ]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons 
              [ askTok1
              , askTok2
              , askTok3
              , askTok4
              , askTok5
              , askTok6
              , askTok7
              , askTok8
              , askTok9
              , askTok10
              , askTok11
              , askTok12
              , askTok13
              , askTok14
              , askTok15
              , askTok16
              , askTok17
              , askTok18
              , askTok19
              , askTok20
              , askTok21
              , askTok22
              , askTok23
              , askTok24
              , askTok25
              , askTok26
              , askTok27
              , askTok28
              , askTok29
              , askTok30
              ]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          , ( Just adaTok3Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok3Name 1
            )
          , ( Just adaTok4Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok4Name 1
            )
          , ( Just adaTok5Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok5Name 1
            )
          , ( Just adaTok6Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok6Name 1
            )
          , ( Just adaTok7Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok7Name 1
            )
          , ( Just adaTok8Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok8Name 1
            )
          , ( Just adaTok9Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok9Name 1
            )
          , ( Just adaTok10Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok10Name 1
            )
          , ( Just adaTok11Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok11Name 1
            )
          , ( Just adaTok12Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok12Name 1
            )
          , ( Just adaTok13Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok13Name 1
            )
          , ( Just adaTok14Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok14Name 1
            )
          , ( Just adaTok15Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok15Name 1
            )
          , ( Just adaTok16Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok16Name 1
            )
          , ( Just adaTok17Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok17Name 1
            )
          , ( Just adaTok18Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok18Name 1
            )
          , ( Just adaTok19Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok19Name 1
            )
          , ( Just adaTok20Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok20Name 1
            )
          , ( Just adaTok21Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok21Name 1
            )
          , ( Just adaTok22Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok22Name 1
            )
          , ( Just adaTok23Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok23Name 1
            )
          , ( Just adaTok24Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok24Name 1
            )
          , ( Just adaTok25Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok25Name 1
            )
          , ( Just adaTok26Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok26Name 1
            )
          , ( Just adaTok27Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok27Name 1
            )
          , ( Just adaTok28Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok28Name 1
            )
          , ( Just adaTok29Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok29Name 1
            )
          , ( Just adaTok30Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok30Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [
            [ (askTok31Name,1)
            , (askTok32Name,1)
            , (askTok33Name,1)
            , (askTok34Name,1)
            , (askTok35Name,1)
            , (askTok36Name,1)
            , (askTok37Name,1)
            , (askTok38Name,1)
            , (askTok39Name,1)
            , (askTok40Name,1)
            , (askTok41Name,1)
            , (askTok42Name,1)
            , (askTok43Name,1)
            , (askTok44Name,1)
            , (askTok45Name,1)
            , (askTok46Name,1)
            , (askTok47Name,1)
            ]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons 
              [ askTok31
              , askTok32
              , askTok33
              , askTok34
              , askTok35
              , askTok36
              , askTok37
              , askTok38
              , askTok39
              , askTok40
              , askTok41
              , askTok42
              , askTok43
              , askTok44
              , askTok45
              , askTok46
              , askTok47
              ]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok31Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok31Name 1
            )
          , ( Just adaTok32Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok32Name 1
            )
          , ( Just adaTok33Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok33Name 1
            )
          , ( Just adaTok34Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok34Name 1
            )
          , ( Just adaTok35Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok35Name 1
            )
          , ( Just adaTok36Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok36Name 1
            )
          , ( Just adaTok37Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok37Name 1
            )
          , ( Just adaTok38Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok38Name 1
            )
          , ( Just adaTok39Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok39Name 1
            )
          , ( Just adaTok40Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok40Name 1
            )
          , ( Just adaTok41Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok41Name 1
            )
          , ( Just adaTok42Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok42Name 1
            )
          , ( Just adaTok43Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok43Name 1
            )
          , ( Just adaTok44Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok44Name 1
            )
          , ( Just adaTok45Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok45Name 1
            )
          , ( Just adaTok46Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok46Name 1
            )
          , ( Just adaTok47Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok47Name 1
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
  input1 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input2 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
  input3 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok3Name 1
  input4 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok4Name 1
  input5 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok5Name 1
  input6 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok6Name 1
  input7 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok7Name 1
  input8 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok8Name 1
  input9 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok9Name 1
  input10 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok10Name 1
  input11 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok11Name 1
  input12 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok12Name 1
  input13 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok13Name 1
  input14 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok14Name 1
  input15 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok15Name 1
  input16 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok16Name 1
  input17 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok17Name 1
  input18 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok18Name 1
  input19 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok19Name 1
  input20 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok20Name 1
  input21 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok21Name 1
  input22 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok22Name 1
  input23 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok23Name 1
  input24 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok24Name 1
  input25 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok25Name 1
  input26 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok26Name 1
  input27 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok27Name 1
  input28 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok28Name 1
  input29 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok29Name 1
  input30 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok30Name 1
  input31 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok31Name 1
  input32 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok32Name 1
  input33 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok33Name 1
  input34 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok34Name 1
  input35 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok35Name 1
  input36 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok36Name 1
  input37 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok37Name 1
  input38 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok38Name 1
  input39 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok39Name 1
  input40 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok40Name 1
  input41 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok41Name 1
  input42 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok42Name 1
  input43 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok43Name 1
  input44 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok44Name 1
  input45 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok45Name 1
  input46 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok46Name 1
  input47 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok47Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = 
          [
            [ (askTok1Name,-1)
            , (askTok2Name,-1)
            , (askTok3Name,-1)
            , (askTok4Name,-1)
            , (askTok5Name,-1)
            , (askTok6Name,-1)
            , (askTok7Name,-1)
            , (askTok8Name,-1)
            , (askTok9Name,-1)
            , (askTok10Name,-1)
            , (askTok11Name,-1)
            , (askTok12Name,-1)
            , (askTok13Name,-1)
            , (askTok14Name,-1)
            , (askTok15Name,-1)
            , (askTok16Name,-1)
            , (askTok17Name,-1)
            , (askTok18Name,-1)
            , (askTok19Name,-1)
            , (askTok20Name,-1)
            , (askTok21Name,-1)
            , (askTok22Name,-1)
            , (askTok23Name,-1)
            , (askTok24Name,-1)
            , (askTok25Name,-1)
            , (askTok26Name,-1)
            , (askTok27Name,-1)
            , (askTok28Name,-1)
            , (askTok29Name,-1)
            , (askTok30Name,-1)
            , (askTok31Name,-1)
            , (askTok32Name,-1)
            , (askTok33Name,-1)
            , (askTok34Name,-1)
            , (askTok35Name,-1)
            , (askTok36Name,-1)
            , (askTok37Name,-1)
            , (askTok38Name,-1)
            , (askTok39Name,-1)
            , (askTok40Name,-1)
            , (askTok41Name,-1)
            , (askTok42Name,-1)
            , (askTok43Name,-1)
            , (askTok44Name,-1)
            , (askTok45Name,-1)
            , (askTok46Name,-1)
            -- , (askTok47Name,-1)
            ]
          ]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ input1
          , input2
          , input3
          , input4
          , input5
          , input6
          , input7
          , input8
          , input9
          , input10
          , input11
          , input12
          , input13
          , input14
          , input15
          , input16
          , input17
          , input18
          , input19
          , input20
          , input21
          , input22
          , input23
          , input24
          , input25
          , input26
          , input27
          , input28
          , input29
          , input30
          , input31
          , input32
          , input33
          , input34
          , input35
          , input36
          , input37
          , input38
          , input39
          , input40
          , input41
          , input42
          , input43
          , input44
          , input45
          , input46
          -- , input47
          ]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = [adaMintRef]
      , closeOrUpdateRefAddress = refAddr
      }

maxCloseForDifferentOffers :: [DappScripts] -> EmulatorTrace ()
maxCloseForDifferentOffers ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
  tok15MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 15
  tok16MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 16
  tok17MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 17
  tok18MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 18
  tok19MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 19
  tok20MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 20
  tok21MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 21
  tok22MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 22
  tok23MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 23
  tok24MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 24
  tok25MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 25
  tok26MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 26
  tok27MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 27
  tok28MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 28
  tok29MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 29
  tok30MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 30

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

      askTok16 = uncurry AssetConfig testToken16
      askTok16Name = genBeaconName askTok16

      askTok17 = uncurry AssetConfig testToken17
      askTok17Name = genBeaconName askTok17

      askTok18 = uncurry AssetConfig testToken18
      askTok18Name = genBeaconName askTok18

      askTok19 = uncurry AssetConfig testToken19
      askTok19Name = genBeaconName askTok19

      askTok20 = uncurry AssetConfig testToken20
      askTok20Name = genBeaconName askTok20

      askTok21 = uncurry AssetConfig testToken21
      askTok21Name = genBeaconName askTok21

      askTok22 = uncurry AssetConfig testToken22
      askTok22Name = genBeaconName askTok22

      askTok23 = uncurry AssetConfig testToken23
      askTok23Name = genBeaconName askTok23

      askTok24 = uncurry AssetConfig testToken24
      askTok24Name = genBeaconName askTok24

      askTok25 = uncurry AssetConfig testToken25
      askTok25Name = genBeaconName askTok25

      askTok26 = uncurry AssetConfig testToken26
      askTok26Name = genBeaconName askTok26

      askTok27 = uncurry AssetConfig testToken27
      askTok27Name = genBeaconName askTok27

      askTok28 = uncurry AssetConfig testToken28
      askTok28Name = genBeaconName askTok28

      askTok29 = uncurry AssetConfig testToken29
      askTok29Name = genBeaconName askTok29

      askTok30 = uncurry AssetConfig testToken30
      askTok30Name = genBeaconName askTok30

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)
      
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

      tok13Tok14Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 13) 
          askTok14Name
          (fst testToken13)
          (snd testToken13)
          (fst testToken14)
          (snd testToken14)
          (unsafeRatio 1 1)
      
      tok14Tok15Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 14) 
          askTok15Name
          (fst testToken14)
          (snd testToken14)
          (fst testToken15)
          (snd testToken15)
          (unsafeRatio 1 1)

      tok15Tok16Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 15) 
          askTok16Name
          (fst testToken15)
          (snd testToken15)
          (fst testToken16)
          (snd testToken16)
          (unsafeRatio 1 1)
      
      tok16Tok17Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 16) 
          askTok17Name
          (fst testToken16)
          (snd testToken16)
          (fst testToken17)
          (snd testToken17)
          (unsafeRatio 1 1)

      tok17Tok18Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 17) 
          askTok18Name
          (fst testToken17)
          (snd testToken17)
          (fst testToken18)
          (snd testToken18)
          (unsafeRatio 1 1)

      tok18Tok19Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 18) 
          askTok19Name
          (fst testToken18)
          (snd testToken18)
          (fst testToken19)
          (snd testToken19)
          (unsafeRatio 1 1)
      
      tok19Tok20Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 19) 
          askTok20Name
          (fst testToken19)
          (snd testToken19)
          (fst testToken20)
          (snd testToken20)
          (unsafeRatio 1 1)

      tok20Tok21Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 20) 
          askTok21Name
          (fst testToken20)
          (snd testToken20)
          (fst testToken21)
          (snd testToken21)
          (unsafeRatio 1 1)

      tok21Tok22Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 21) 
          askTok22Name
          (fst testToken21)
          (snd testToken21)
          (fst testToken22)
          (snd testToken22)
          (unsafeRatio 1 1)

      tok22Tok23Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 22) 
          askTok23Name
          (fst testToken22)
          (snd testToken22)
          (fst testToken23)
          (snd testToken23)
          (unsafeRatio 1 1)

      tok23Tok24Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 23) 
          askTok24Name
          (fst testToken23)
          (snd testToken23)
          (fst testToken24)
          (snd testToken24)
          (unsafeRatio 1 1)
      
      tok24Tok25Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 24) 
          askTok25Name
          (fst testToken24)
          (snd testToken24)
          (fst testToken25)
          (snd testToken25)
          (unsafeRatio 1 1)

      tok25Tok26Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 25) 
          askTok26Name
          (fst testToken25)
          (snd testToken25)
          (fst testToken26)
          (snd testToken26)
          (unsafeRatio 1 1)
      
      tok26Tok27Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 26) 
          askTok27Name
          (fst testToken26)
          (snd testToken26)
          (fst testToken27)
          (snd testToken27)
          (unsafeRatio 1 1)

      tok27Tok28Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 27) 
          askTok28Name
          (fst testToken27)
          (snd testToken27)
          (fst testToken28)
          (snd testToken28)
          (unsafeRatio 1 1)

      tok28Tok29Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 28) 
          askTok29Name
          (fst testToken28)
          (snd testToken28)
          (fst testToken29)
          (snd testToken29)
          (unsafeRatio 1 1)
      
      tok29Tok30Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 29) 
          askTok30Name
          (fst testToken29)
          (snd testToken29)
          (fst testToken30)
          (snd testToken30)
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
          , [(askTok14Name,1)]
          , [(askTok15Name,1)]
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
          , MintBeacons [askTok14]
          , MintBeacons [askTok15]
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
          , ( Just tok13Tok14Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!13) askTok14Name 1
           <> (uncurry singleton testToken13) 10
            )
          , ( Just tok14Tok15Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!14) askTok15Name 1
           <> (uncurry singleton testToken14) 10
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
          , tok14MintRef
          , tok15MintRef
          ]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [ [(askTok16Name,1)]
          , [(askTok17Name,1)]
          , [(askTok18Name,1)]
          , [(askTok19Name,1)]
          , [(askTok20Name,1)]
          , [(askTok21Name,1)]
          , [(askTok22Name,1)]
          , [(askTok23Name,1)]
          , [(askTok24Name,1)]
          , [(askTok25Name,1)]
          , [(askTok26Name,1)]
          , [(askTok27Name,1)]
          , [(askTok28Name,1)]
          , [(askTok29Name,1)]
          , [(askTok30Name,1)]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons [askTok16]
          , MintBeacons [askTok17]
          , MintBeacons [askTok18]
          , MintBeacons [askTok19]
          , MintBeacons [askTok20]
          , MintBeacons [askTok21]
          , MintBeacons [askTok22]
          , MintBeacons [askTok23]
          , MintBeacons [askTok24]
          , MintBeacons [askTok25]
          , MintBeacons [askTok26]
          , MintBeacons [askTok27]
          , MintBeacons [askTok28]
          , MintBeacons [askTok29]
          , MintBeacons [askTok30]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just tok15Tok16Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!15) askTok16Name 1
           <> (uncurry singleton testToken15) 10
            )
          , ( Just tok16Tok17Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!16) askTok17Name 1
           <> (uncurry singleton testToken16) 10
            )
          , ( Just tok17Tok18Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!17) askTok18Name 1
           <> (uncurry singleton testToken17) 10
            )
          , ( Just tok18Tok19Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!18) askTok19Name 1
           <> (uncurry singleton testToken18) 10
            )
          , ( Just tok19Tok20Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!19) askTok20Name 1
           <> (uncurry singleton testToken19) 10
            )
          , ( Just tok20Tok21Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!20) askTok21Name 1
           <> (uncurry singleton testToken20) 10
            )
          , ( Just tok21Tok22Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!21) askTok22Name 1
           <> (uncurry singleton testToken21) 10
            )
          , ( Just tok22Tok23Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!22) askTok23Name 1
           <> (uncurry singleton testToken22) 10
            )
          , ( Just tok23Tok24Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!23) askTok24Name 1
           <> (uncurry singleton testToken23) 10
            )
          , ( Just tok24Tok25Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!24) askTok25Name 1
           <> (uncurry singleton testToken24) 10
            )
          , ( Just tok25Tok26Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!25) askTok26Name 1
           <> (uncurry singleton testToken25) 10
            )
          , ( Just tok26Tok27Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!26) askTok27Name 1
           <> (uncurry singleton testToken26) 10
            )
          , ( Just tok27Tok28Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!27) askTok28Name 1
           <> (uncurry singleton testToken27) 10
            )
          , ( Just tok28Tok29Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!28) askTok29Name 1
           <> (uncurry singleton testToken28) 10
            )
          , ( Just tok29Tok30Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!29) askTok30Name 1
           <> (uncurry singleton testToken29) 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = drop 15 ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = 
          [ tok15MintRef
          , tok16MintRef
          , tok17MintRef
          , tok18MintRef
          , tok19MintRef
          , tok20MintRef
          , tok21MintRef
          , tok22MintRef
          , tok23MintRef
          , tok24MintRef
          , tok25MintRef
          , tok26MintRef
          , tok27MintRef
          , tok28MintRef
          , tok29MintRef
          , tok30MintRef
          ]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  input1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input2 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                             <> uncurry singleton testToken1 10
  input3 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
                             <> uncurry singleton testToken2 10
  input4 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!3) askTok4Name 1
                             <> uncurry singleton testToken3 10
  input5 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!4) askTok5Name 1
                             <> uncurry singleton testToken4 10
  input6 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!5) askTok6Name 1
                             <> uncurry singleton testToken5 10
  input7 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!6) askTok7Name 1
                             <> uncurry singleton testToken6 10
  input8 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!7) askTok8Name 1
                             <> uncurry singleton testToken7 10
  input9 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!8) askTok9Name 1
                             <> uncurry singleton testToken8 10
  input10 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!9) askTok10Name 1
                             <> uncurry singleton testToken9 10
  input11 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!10) askTok11Name 1
                             <> uncurry singleton testToken10 10
  input12 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!11) askTok12Name 1
                             <> uncurry singleton testToken11 10
  input13 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!12) askTok13Name 1
                             <> uncurry singleton testToken12 10
  input14 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!13) askTok14Name 1
                             <> uncurry singleton testToken13 10
  input15 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!14) askTok15Name 1
                             <> uncurry singleton testToken14 10
  input16 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!15) askTok16Name 1
                             <> uncurry singleton testToken15 10
  input17 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!16) askTok17Name 1
                             <> uncurry singleton testToken16 10
  input18 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!17) askTok18Name 1
                             <> uncurry singleton testToken17 10
  input19 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!18) askTok19Name 1
                             <> uncurry singleton testToken18 10
  input20 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!19) askTok20Name 1
                             <> uncurry singleton testToken19 10
  input21 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!20) askTok21Name 1
                             <> uncurry singleton testToken20 10
  input22 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!21) askTok22Name 1
                             <> uncurry singleton testToken21 10
  input23 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!22) askTok23Name 1
                             <> uncurry singleton testToken22 10
  input24 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!23) askTok24Name 1
                             <> uncurry singleton testToken23 10
  input25 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!24) askTok25Name 1
                             <> uncurry singleton testToken24 10
  input26 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!25) askTok26Name 1
                             <> uncurry singleton testToken25 10
  input27 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!26) askTok27Name 1
                             <> uncurry singleton testToken26 10
  input28 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!27) askTok28Name 1
                             <> uncurry singleton testToken27 10
  input29 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!28) askTok29Name 1
                             <> uncurry singleton testToken28 10
  input30 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!29) askTok30Name 1
                             <> uncurry singleton testToken29 10
  
  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = 
          [ [(askTok1Name,-1)]
          , [(askTok2Name,-1)]
          , [(askTok3Name,-1)]
          , [(askTok4Name,-1)]
          , [(askTok5Name,-1)]
          , [(askTok6Name,-1)]
          , [(askTok7Name,-1)]
          , [(askTok8Name,-1)]
          , [(askTok9Name,-1)]
          , [(askTok10Name,-1)]
          , [(askTok11Name,-1)]
          , [(askTok12Name,-1)]
          , [(askTok13Name,-1)]
          , [(askTok14Name,-1)]
          , [(askTok15Name,-1)]
          , [(askTok16Name,-1)]
          , [(askTok17Name,-1)]
          , [(askTok18Name,-1)]
          , [(askTok19Name,-1)]
          , [(askTok20Name,-1)]
          , [(askTok21Name,-1)]
          , [(askTok22Name,-1)]
          , [(askTok23Name,-1)]
          , [(askTok24Name,-1)]
          , [(askTok25Name,-1)]
          , [(askTok26Name,-1)]
          , [(askTok27Name,-1)]
          , [(askTok28Name,-1)]
          , [(askTok29Name,-1)]
          , [(askTok30Name,-1)]
          ]
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ input1
          , input2
          , input3
          , input4
          , input5
          , input6
          , input7
          , input8
          , input9
          , input10
          , input11
          , input12
          , input13
          , input14
          , input15
          , input16
          , input17
          , input18
          , input19
          , input20
          , input21
          , input22
          , input23
          , input24
          , input25
          , input26
          , input27
          , input28
          , input29
          , input30
          ]
      , closeOrUpdateNewSwaps = []
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = 
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
          , tok14MintRef
          , tok15MintRef
          , tok16MintRef
          , tok17MintRef
          , tok18MintRef
          , tok19MintRef
          , tok20MintRef
          , tok21MintRef
          , tok22MintRef
          , tok23MintRef
          , tok24MintRef
          , tok25MintRef
          , tok26MintRef
          , tok27MintRef
          , tok28MintRef
          , tok29MintRef
          -- , tok30MintRef
          ]
      , closeOrUpdateRefAddress = refAddr
      }

maxUpdateForSameTradingPair :: [DappScripts] -> EmulatorTrace ()
maxUpdateForSameTradingPair ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
      { createSwapBeaconsMinted = [[(askTok1Name,50)]]
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
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_011
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_012
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_013
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_014
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_015
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_016
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_017
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_018
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_019
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_020
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_021
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_022
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_023
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_024
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_025
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_026
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_027
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_028
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_029
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_030
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_031
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_032
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_033
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_034
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_035
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_036
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_037
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_038
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_039
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_040
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_041
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_042
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_043
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_044
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_045
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_046
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_047
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_048
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_049
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 5_000_050
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
  input11 <- txOutRefWithValue $ lovelaceValueOf 5_000_011
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input12 <- txOutRefWithValue $ lovelaceValueOf 5_000_012
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input13 <- txOutRefWithValue $ lovelaceValueOf 5_000_013
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input14 <- txOutRefWithValue $ lovelaceValueOf 5_000_014
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input15 <- txOutRefWithValue $ lovelaceValueOf 5_000_015
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input16 <- txOutRefWithValue $ lovelaceValueOf 5_000_016
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input17 <- txOutRefWithValue $ lovelaceValueOf 5_000_017
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input18 <- txOutRefWithValue $ lovelaceValueOf 5_000_018
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input19 <- txOutRefWithValue $ lovelaceValueOf 5_000_019
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input20 <- txOutRefWithValue $ lovelaceValueOf 5_000_020
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input21 <- txOutRefWithValue $ lovelaceValueOf 5_000_021
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input22 <- txOutRefWithValue $ lovelaceValueOf 5_000_022
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input23 <- txOutRefWithValue $ lovelaceValueOf 5_000_023
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input24 <- txOutRefWithValue $ lovelaceValueOf 5_000_024
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input25 <- txOutRefWithValue $ lovelaceValueOf 5_000_025
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input26 <- txOutRefWithValue $ lovelaceValueOf 5_000_026
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input27 <- txOutRefWithValue $ lovelaceValueOf 5_000_027
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input28 <- txOutRefWithValue $ lovelaceValueOf 5_000_028
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input29 <- txOutRefWithValue $ lovelaceValueOf 5_000_029
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input30 <- txOutRefWithValue $ lovelaceValueOf 5_000_030
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input31 <- txOutRefWithValue $ lovelaceValueOf 5_000_031
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input32 <- txOutRefWithValue $ lovelaceValueOf 5_000_032
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input33 <- txOutRefWithValue $ lovelaceValueOf 5_000_033
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input34 <- txOutRefWithValue $ lovelaceValueOf 5_000_034
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input35 <- txOutRefWithValue $ lovelaceValueOf 5_000_035
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input36 <- txOutRefWithValue $ lovelaceValueOf 5_000_036
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input37 <- txOutRefWithValue $ lovelaceValueOf 5_000_037
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input38 <- txOutRefWithValue $ lovelaceValueOf 5_000_038
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input39 <- txOutRefWithValue $ lovelaceValueOf 5_000_039
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input40 <- txOutRefWithValue $ lovelaceValueOf 5_000_040
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input41 <- txOutRefWithValue $ lovelaceValueOf 5_000_041
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input42 <- txOutRefWithValue $ lovelaceValueOf 5_000_042
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input43 <- txOutRefWithValue $ lovelaceValueOf 5_000_043
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input44 <- txOutRefWithValue $ lovelaceValueOf 5_000_044
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input45 <- txOutRefWithValue $ lovelaceValueOf 5_000_045
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input46 <- txOutRefWithValue $ lovelaceValueOf 5_000_046
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input47 <- txOutRefWithValue $ lovelaceValueOf 5_000_047
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input48 <- txOutRefWithValue $ lovelaceValueOf 5_000_048
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input49 <- txOutRefWithValue $ lovelaceValueOf 5_000_049
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input50 <- txOutRefWithValue $ lovelaceValueOf 5_000_050
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ input1
          -- , input2
          -- , input3
          -- , input4
          -- , input5
          -- , input6
          -- , input7
          -- , input8
          -- , input9
          -- , input10
          -- , input11
          -- , input12
          -- , input13
          -- , input14
          -- , input15
          -- , input16
          -- , input17
          -- , input18
          -- , input19
          -- , input20
          -- , input21
          -- , input22
          -- , input23
          -- , input24
          -- , input25
          -- , input26
          -- , input27
          -- , input28
          -- , input29
          -- , input30
          -- , input31
          -- , input32
          -- , input33
          -- , input34
          -- , input35
          -- , input36
          -- , input37
          -- , input38
          -- , input39
          -- , input40
          -- , input41
          -- , input42
          -- , input43
          -- , input44
          -- , input45
          -- , input46
          -- , input47
          -- , input48
          -- , input49
          -- , input50
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum
            , lovelaceValueOf 5_000_001
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_002
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_003
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_004
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_005
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_006
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_007
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_008
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_009
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_010
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_011
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_012
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_013
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_014
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_015
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_016
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          -- , ( Just adaTok1Datum
          --   , lovelaceValueOf 5_000_017
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
          --   )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = []
      , closeOrUpdateRefAddress = refAddr
      }

maxUpdateForSameOfferButDifferentAsks :: [DappScripts] -> EmulatorTrace ()
maxUpdateForSameOfferButDifferentAsks ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

      askTok16 = uncurry AssetConfig testToken16
      askTok16Name = genBeaconName askTok16

      askTok17 = uncurry AssetConfig testToken17
      askTok17Name = genBeaconName askTok17

      askTok18 = uncurry AssetConfig testToken18
      askTok18Name = genBeaconName askTok18

      askTok19 = uncurry AssetConfig testToken19
      askTok19Name = genBeaconName askTok19

      askTok20 = uncurry AssetConfig testToken20
      askTok20Name = genBeaconName askTok20

      askTok21 = uncurry AssetConfig testToken21
      askTok21Name = genBeaconName askTok21

      askTok22 = uncurry AssetConfig testToken22
      askTok22Name = genBeaconName askTok22

      askTok23 = uncurry AssetConfig testToken23
      askTok23Name = genBeaconName askTok23

      askTok24 = uncurry AssetConfig testToken24
      askTok24Name = genBeaconName askTok24

      askTok25 = uncurry AssetConfig testToken25
      askTok25Name = genBeaconName askTok25

      askTok26 = uncurry AssetConfig testToken26
      askTok26Name = genBeaconName askTok26

      askTok27 = uncurry AssetConfig testToken27
      askTok27Name = genBeaconName askTok27

      askTok28 = uncurry AssetConfig testToken28
      askTok28Name = genBeaconName askTok28

      askTok29 = uncurry AssetConfig testToken29
      askTok29Name = genBeaconName askTok29

      askTok30 = uncurry AssetConfig testToken30
      askTok30Name = genBeaconName askTok30

      askTok31 = uncurry AssetConfig testToken31
      askTok31Name = genBeaconName askTok31

      askTok32 = uncurry AssetConfig testToken32
      askTok32Name = genBeaconName askTok32

      askTok33 = uncurry AssetConfig testToken33
      askTok33Name = genBeaconName askTok33

      askTok34 = uncurry AssetConfig testToken34
      askTok34Name = genBeaconName askTok34

      askTok35 = uncurry AssetConfig testToken35
      askTok35Name = genBeaconName askTok35

      askTok36 = uncurry AssetConfig testToken36
      askTok36Name = genBeaconName askTok36

      askTok37 = uncurry AssetConfig testToken37
      askTok37Name = genBeaconName askTok37

      askTok38 = uncurry AssetConfig testToken38
      askTok38Name = genBeaconName askTok38

      askTok39 = uncurry AssetConfig testToken39
      askTok39Name = genBeaconName askTok39

      askTok40 = uncurry AssetConfig testToken40
      askTok40Name = genBeaconName askTok40

      askTok41 = uncurry AssetConfig testToken41
      askTok41Name = genBeaconName askTok41

      askTok42 = uncurry AssetConfig testToken42
      askTok42Name = genBeaconName askTok42

      askTok43 = uncurry AssetConfig testToken43
      askTok43Name = genBeaconName askTok43

      askTok44 = uncurry AssetConfig testToken44
      askTok44Name = genBeaconName askTok44

      askTok45 = uncurry AssetConfig testToken45
      askTok45Name = genBeaconName askTok45

      askTok46 = uncurry AssetConfig testToken46
      askTok46Name = genBeaconName askTok46

      askTok47 = uncurry AssetConfig testToken47
      askTok47Name = genBeaconName askTok47

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1)

      adaTok3Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok3Name
          ""
          ""
          (fst testToken3)
          (snd testToken3)
          (unsafeRatio 1 1)

      adaTok4Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok4Name
          ""
          ""
          (fst testToken4)
          (snd testToken4)
          (unsafeRatio 1 1)
      
      adaTok5Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok5Name
          ""
          ""
          (fst testToken5)
          (snd testToken5)
          (unsafeRatio 1 1)

      adaTok6Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok6Name
          ""
          ""
          (fst testToken6)
          (snd testToken6)
          (unsafeRatio 1 1)

      adaTok7Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok7Name
          ""
          ""
          (fst testToken7)
          (snd testToken7)
          (unsafeRatio 1 1)
        
      adaTok8Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok8Name
          ""
          ""
          (fst testToken8)
          (snd testToken8)
          (unsafeRatio 1 1)
      
      adaTok9Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok9Name
          ""
          ""
          (fst testToken9)
          (snd testToken9)
          (unsafeRatio 1 1)

      adaTok10Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok10Name
          ""
          ""
          (fst testToken10)
          (snd testToken10)
          (unsafeRatio 1 1)
      
      adaTok11Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok11Name
          ""
          ""
          (fst testToken11)
          (snd testToken11)
          (unsafeRatio 1 1)

      adaTok12Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok12Name
          ""
          ""
          (fst testToken12)
          (snd testToken12)
          (unsafeRatio 1 1)

      adaTok13Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok13Name
          ""
          ""
          (fst testToken13)
          (snd testToken13)
          (unsafeRatio 1 1)

      adaTok14Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok14Name
          ""
          ""
          (fst testToken14)
          (snd testToken14)
          (unsafeRatio 1 1)

      adaTok15Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok15Name
          ""
          ""
          (fst testToken15)
          (snd testToken15)
          (unsafeRatio 1 1)

      adaTok16Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok16Name
          ""
          ""
          (fst testToken16)
          (snd testToken16)
          (unsafeRatio 1 1)

      adaTok17Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok17Name
          ""
          ""
          (fst testToken17)
          (snd testToken17)
          (unsafeRatio 1 1)
      
      adaTok18Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok18Name
          ""
          ""
          (fst testToken18)
          (snd testToken18)
          (unsafeRatio 1 1)

      adaTok19Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok19Name
          ""
          ""
          (fst testToken19)
          (snd testToken19)
          (unsafeRatio 1 1)

      adaTok20Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok20Name
          ""
          ""
          (fst testToken20)
          (snd testToken20)
          (unsafeRatio 1 1)

      adaTok21Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok21Name
          ""
          ""
          (fst testToken21)
          (snd testToken21)
          (unsafeRatio 10 1_000_000)

      adaTok22Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok22Name
          ""
          ""
          (fst testToken22)
          (snd testToken22)
          (unsafeRatio 1 1)

      adaTok23Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok23Name
          ""
          ""
          (fst testToken23)
          (snd testToken23)
          (unsafeRatio 1 1)

      adaTok24Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok24Name
          ""
          ""
          (fst testToken24)
          (snd testToken24)
          (unsafeRatio 1 1)
      
      adaTok25Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok25Name
          ""
          ""
          (fst testToken25)
          (snd testToken25)
          (unsafeRatio 1 1)

      adaTok26Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok26Name
          ""
          ""
          (fst testToken26)
          (snd testToken26)
          (unsafeRatio 1 1)

      adaTok27Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok27Name
          ""
          ""
          (fst testToken27)
          (snd testToken27)
          (unsafeRatio 1 1)
        
      adaTok28Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok28Name
          ""
          ""
          (fst testToken28)
          (snd testToken28)
          (unsafeRatio 1 1)
      
      adaTok29Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok29Name
          ""
          ""
          (fst testToken29)
          (snd testToken29)
          (unsafeRatio 1 1)

      adaTok30Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok30Name
          ""
          ""
          (fst testToken30)
          (snd testToken30)
          (unsafeRatio 1 1)
      
      adaTok31Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok31Name
          ""
          ""
          (fst testToken31)
          (snd testToken31)
          (unsafeRatio 1 1)

      adaTok32Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok32Name
          ""
          ""
          (fst testToken32)
          (snd testToken32)
          (unsafeRatio 1 1)

      adaTok33Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok33Name
          ""
          ""
          (fst testToken33)
          (snd testToken33)
          (unsafeRatio 1 1)

      adaTok34Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok34Name
          ""
          ""
          (fst testToken34)
          (snd testToken34)
          (unsafeRatio 1 1)

      adaTok35Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok35Name
          ""
          ""
          (fst testToken35)
          (snd testToken35)
          (unsafeRatio 1 1)

      adaTok36Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok36Name
          ""
          ""
          (fst testToken36)
          (snd testToken36)
          (unsafeRatio 1 1)

      adaTok37Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok37Name
          ""
          ""
          (fst testToken37)
          (snd testToken37)
          (unsafeRatio 1 1)
      
      adaTok38Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok38Name
          ""
          ""
          (fst testToken38)
          (snd testToken38)
          (unsafeRatio 1 1)

      adaTok39Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok39Name
          ""
          ""
          (fst testToken39)
          (snd testToken39)
          (unsafeRatio 1 1)

      adaTok40Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok40Name
          ""
          ""
          (fst testToken40)
          (snd testToken40)
          (unsafeRatio 1 1)

      adaTok41Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok41Name
          ""
          ""
          (fst testToken41)
          (snd testToken41)
          (unsafeRatio 1 1)

      adaTok42Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok42Name
          ""
          ""
          (fst testToken42)
          (snd testToken42)
          (unsafeRatio 1 1)

      adaTok43Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok43Name
          ""
          ""
          (fst testToken43)
          (snd testToken43)
          (unsafeRatio 1 1)

      adaTok44Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok44Name
          ""
          ""
          (fst testToken44)
          (snd testToken44)
          (unsafeRatio 1 1)

      adaTok45Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok45Name
          ""
          ""
          (fst testToken45)
          (snd testToken45)
          (unsafeRatio 1 1)

      adaTok46Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok46Name
          ""
          ""
          (fst testToken46)
          (snd testToken46)
          (unsafeRatio 1 1)

      adaTok47Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok47Name
          ""
          ""
          (fst testToken47)
          (snd testToken47)
          (unsafeRatio 1 1)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [
            [ (askTok1Name,1)
            , (askTok2Name,1)
            , (askTok3Name,1)
            , (askTok4Name,1)
            , (askTok5Name,1)
            , (askTok6Name,1)
            , (askTok7Name,1)
            , (askTok8Name,1)
            , (askTok9Name,1)
            , (askTok10Name,1)
            , (askTok11Name,1)
            , (askTok12Name,1)
            , (askTok13Name,1)
            , (askTok14Name,1)
            , (askTok15Name,1)
            , (askTok16Name,1)
            , (askTok17Name,1)
            , (askTok18Name,1)
            , (askTok19Name,1)
            , (askTok20Name,1)
            , (askTok21Name,1)
            , (askTok22Name,1)
            , (askTok23Name,1)
            , (askTok24Name,1)
            , (askTok25Name,1)
            , (askTok26Name,1)
            , (askTok27Name,1)
            , (askTok28Name,1)
            , (askTok29Name,1)
            , (askTok30Name,1)
            ]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons 
              [ askTok1
              , askTok2
              , askTok3
              , askTok4
              , askTok5
              , askTok6
              , askTok7
              , askTok8
              , askTok9
              , askTok10
              , askTok11
              , askTok12
              , askTok13
              , askTok14
              , askTok15
              , askTok16
              , askTok17
              , askTok18
              , askTok19
              , askTok20
              , askTok21
              , askTok22
              , askTok23
              , askTok24
              , askTok25
              , askTok26
              , askTok27
              , askTok28
              , askTok29
              , askTok30
              ]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          , ( Just adaTok3Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok3Name 1
            )
          , ( Just adaTok4Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok4Name 1
            )
          , ( Just adaTok5Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok5Name 1
            )
          , ( Just adaTok6Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok6Name 1
            )
          , ( Just adaTok7Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok7Name 1
            )
          , ( Just adaTok8Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok8Name 1
            )
          , ( Just adaTok9Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok9Name 1
            )
          , ( Just adaTok10Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok10Name 1
            )
          , ( Just adaTok11Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok11Name 1
            )
          , ( Just adaTok12Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok12Name 1
            )
          , ( Just adaTok13Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok13Name 1
            )
          , ( Just adaTok14Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok14Name 1
            )
          , ( Just adaTok15Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok15Name 1
            )
          , ( Just adaTok16Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok16Name 1
            )
          , ( Just adaTok17Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok17Name 1
            )
          , ( Just adaTok18Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok18Name 1
            )
          , ( Just adaTok19Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok19Name 1
            )
          , ( Just adaTok20Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok20Name 1
            )
          , ( Just adaTok21Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok21Name 1
            )
          , ( Just adaTok22Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok22Name 1
            )
          , ( Just adaTok23Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok23Name 1
            )
          , ( Just adaTok24Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok24Name 1
            )
          , ( Just adaTok25Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok25Name 1
            )
          , ( Just adaTok26Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok26Name 1
            )
          , ( Just adaTok27Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok27Name 1
            )
          , ( Just adaTok28Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok28Name 1
            )
          , ( Just adaTok29Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok29Name 1
            )
          , ( Just adaTok30Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok30Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [
            [ (askTok31Name,1)
            , (askTok32Name,1)
            , (askTok33Name,1)
            , (askTok34Name,1)
            , (askTok35Name,1)
            , (askTok36Name,1)
            , (askTok37Name,1)
            , (askTok38Name,1)
            , (askTok39Name,1)
            , (askTok40Name,1)
            , (askTok41Name,1)
            , (askTok42Name,1)
            , (askTok43Name,1)
            , (askTok44Name,1)
            , (askTok45Name,1)
            , (askTok46Name,1)
            , (askTok47Name,1)
            ]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons 
              [ askTok31
              , askTok32
              , askTok33
              , askTok34
              , askTok35
              , askTok36
              , askTok37
              , askTok38
              , askTok39
              , askTok40
              , askTok41
              , askTok42
              , askTok43
              , askTok44
              , askTok45
              , askTok46
              , askTok47
              ]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok31Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok31Name 1
            )
          , ( Just adaTok32Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok32Name 1
            )
          , ( Just adaTok33Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok33Name 1
            )
          , ( Just adaTok34Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok34Name 1
            )
          , ( Just adaTok35Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok35Name 1
            )
          , ( Just adaTok36Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok36Name 1
            )
          , ( Just adaTok37Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok37Name 1
            )
          , ( Just adaTok38Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok38Name 1
            )
          , ( Just adaTok39Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok39Name 1
            )
          , ( Just adaTok40Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok40Name 1
            )
          , ( Just adaTok41Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok41Name 1
            )
          , ( Just adaTok42Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok42Name 1
            )
          , ( Just adaTok43Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok43Name 1
            )
          , ( Just adaTok44Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok44Name 1
            )
          , ( Just adaTok45Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok45Name 1
            )
          , ( Just adaTok46Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok46Name 1
            )
          , ( Just adaTok47Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok47Name 1
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
  input1 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input2 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
  input3 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok3Name 1
  input4 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok4Name 1
  input5 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok5Name 1
  input6 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok6Name 1
  input7 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok7Name 1
  input8 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok8Name 1
  input9 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok9Name 1
  input10 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok10Name 1
  input11 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok11Name 1
  input12 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok12Name 1
  input13 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok13Name 1
  input14 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok14Name 1
  input15 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok15Name 1
  input16 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok16Name 1
  input17 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok17Name 1
  input18 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok18Name 1
  input19 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok19Name 1
  input20 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok20Name 1
  input21 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok21Name 1
  input22 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok22Name 1
  input23 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok23Name 1
  input24 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok24Name 1
  input25 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok25Name 1
  input26 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok26Name 1
  input27 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok27Name 1
  input28 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok28Name 1
  input29 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok29Name 1
  input30 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok30Name 1
  input31 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok31Name 1
  input32 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok32Name 1
  input33 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok33Name 1
  input34 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok34Name 1
  input35 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok35Name 1
  input36 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok36Name 1
  input37 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok37Name 1
  input38 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok38Name 1
  input39 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok39Name 1
  input40 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok40Name 1
  input41 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok41Name 1
  input42 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok42Name 1
  input43 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok43Name 1
  input44 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok44Name 1
  input45 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok45Name 1
  input46 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok46Name 1
  input47 <- txOutRefWithValue $ lovelaceValueOf 5_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok47Name 1

  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ input1
          , input2
          , input3
          , input4
          , input5
          , input6
          , input7
          , input8
          , input9
          , input10
          , input11
          , input12
          , input13
          , input14
          , input15
          -- , input16
          -- , input17
          -- , input18
          -- , input19
          -- , input20
          -- , input21
          -- , input22
          -- , input23
          -- , input24
          -- , input25
          -- , input26
          -- , input27
          -- , input28
          -- , input29
          -- , input30
          -- , input31
          -- , input32
          -- , input33
          -- , input34
          -- , input35
          -- , input36
          -- , input37
          -- , input38
          -- , input39
          -- , input40
          -- , input41
          -- , input42
          -- , input43
          -- , input44
          -- , input45
          -- , input46
          -- , input47
          ]
      , closeOrUpdateNewSwaps =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          , ( Just adaTok3Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok3Name 1
            )
          , ( Just adaTok4Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok4Name 1
            )
          , ( Just adaTok5Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok5Name 1
            )
          , ( Just adaTok6Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok6Name 1
            )
          , ( Just adaTok7Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok7Name 1
            )
          , ( Just adaTok8Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok8Name 1
            )
          , ( Just adaTok9Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok9Name 1
            )
          , ( Just adaTok10Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok10Name 1
            )
          , ( Just adaTok11Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok11Name 1
            )
          , ( Just adaTok12Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok12Name 1
            )
          , ( Just adaTok13Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok13Name 1
            )
          , ( Just adaTok14Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok14Name 1
            )
          , ( Just adaTok15Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok15Name 1
            )
          -- , ( Just adaTok16Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok16Name 1
          --   )
          -- , ( Just adaTok17Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok17Name 1
          --   )
          -- , ( Just adaTok18Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok18Name 1
          --   )
          -- , ( Just adaTok19Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok19Name 1
          --   )
          -- , ( Just adaTok20Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok20Name 1
          --   )
          -- , ( Just adaTok21Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok21Name 1
          --   )
          -- , ( Just adaTok22Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok22Name 1
          --   )
          -- , ( Just adaTok23Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok23Name 1
          --   )
          -- , ( Just adaTok24Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok24Name 1
          --   )
          -- , ( Just adaTok25Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok25Name 1
          --   )
          -- , ( Just adaTok26Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok26Name 1
          --   )
          -- , ( Just adaTok27Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok27Name 1
          --   )
          -- , ( Just adaTok28Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok28Name 1
          --   )
          -- , ( Just adaTok29Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok29Name 1
          --   )
          -- , ( Just adaTok30Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok30Name 1
          --   )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = []
      , closeOrUpdateRefAddress = refAddr
      }

maxUpdateForDifferentOffers :: [DappScripts] -> EmulatorTrace ()
maxUpdateForDifferentOffers ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
  tok15MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 15
  tok16MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 16
  tok17MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 17
  tok18MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 18
  tok19MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 19
  tok20MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 20
  tok21MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 21
  tok22MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 22
  tok23MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 23
  tok24MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 24
  tok25MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 25
  tok26MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 26
  tok27MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 27
  tok28MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 28
  tok29MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 29
  tok30MintRef <- txOutRefWithValue $ lovelaceValueOf $ minUTxOMintRef + 30

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

      askTok16 = uncurry AssetConfig testToken16
      askTok16Name = genBeaconName askTok16

      askTok17 = uncurry AssetConfig testToken17
      askTok17Name = genBeaconName askTok17

      askTok18 = uncurry AssetConfig testToken18
      askTok18Name = genBeaconName askTok18

      askTok19 = uncurry AssetConfig testToken19
      askTok19Name = genBeaconName askTok19

      askTok20 = uncurry AssetConfig testToken20
      askTok20Name = genBeaconName askTok20

      askTok21 = uncurry AssetConfig testToken21
      askTok21Name = genBeaconName askTok21

      askTok22 = uncurry AssetConfig testToken22
      askTok22Name = genBeaconName askTok22

      askTok23 = uncurry AssetConfig testToken23
      askTok23Name = genBeaconName askTok23

      askTok24 = uncurry AssetConfig testToken24
      askTok24Name = genBeaconName askTok24

      askTok25 = uncurry AssetConfig testToken25
      askTok25Name = genBeaconName askTok25

      askTok26 = uncurry AssetConfig testToken26
      askTok26Name = genBeaconName askTok26

      askTok27 = uncurry AssetConfig testToken27
      askTok27Name = genBeaconName askTok27

      askTok28 = uncurry AssetConfig testToken28
      askTok28Name = genBeaconName askTok28

      askTok29 = uncurry AssetConfig testToken29
      askTok29Name = genBeaconName askTok29

      askTok30 = uncurry AssetConfig testToken30
      askTok30Name = genBeaconName askTok30

      adaTok1Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok1Name
          ""
          ""
          (fst testToken1)
          (snd testToken1)
          (unsafeRatio 10 1_000_000)
      
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

      tok13Tok14Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 13) 
          askTok14Name
          (fst testToken13)
          (snd testToken13)
          (fst testToken14)
          (snd testToken14)
          (unsafeRatio 1 1)
      
      tok14Tok15Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 14) 
          askTok15Name
          (fst testToken14)
          (snd testToken14)
          (fst testToken15)
          (snd testToken15)
          (unsafeRatio 1 1)

      tok15Tok16Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 15) 
          askTok16Name
          (fst testToken15)
          (snd testToken15)
          (fst testToken16)
          (snd testToken16)
          (unsafeRatio 1 1)
      
      tok16Tok17Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 16) 
          askTok17Name
          (fst testToken16)
          (snd testToken16)
          (fst testToken17)
          (snd testToken17)
          (unsafeRatio 1 1)

      tok17Tok18Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 17) 
          askTok18Name
          (fst testToken17)
          (snd testToken17)
          (fst testToken18)
          (snd testToken18)
          (unsafeRatio 1 1)

      tok18Tok19Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 18) 
          askTok19Name
          (fst testToken18)
          (snd testToken18)
          (fst testToken19)
          (snd testToken19)
          (unsafeRatio 1 1)
      
      tok19Tok20Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 19) 
          askTok20Name
          (fst testToken19)
          (snd testToken19)
          (fst testToken20)
          (snd testToken20)
          (unsafeRatio 1 1)

      tok20Tok21Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 20) 
          askTok21Name
          (fst testToken20)
          (snd testToken20)
          (fst testToken21)
          (snd testToken21)
          (unsafeRatio 1 1)

      tok21Tok22Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 21) 
          askTok22Name
          (fst testToken21)
          (snd testToken21)
          (fst testToken22)
          (snd testToken22)
          (unsafeRatio 1 1)

      tok22Tok23Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 22) 
          askTok23Name
          (fst testToken22)
          (snd testToken22)
          (fst testToken23)
          (snd testToken23)
          (unsafeRatio 1 1)

      tok23Tok24Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 23) 
          askTok24Name
          (fst testToken23)
          (snd testToken23)
          (fst testToken24)
          (snd testToken24)
          (unsafeRatio 1 1)
      
      tok24Tok25Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 24) 
          askTok25Name
          (fst testToken24)
          (snd testToken24)
          (fst testToken25)
          (snd testToken25)
          (unsafeRatio 1 1)

      tok25Tok26Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 25) 
          askTok26Name
          (fst testToken25)
          (snd testToken25)
          (fst testToken26)
          (snd testToken26)
          (unsafeRatio 1 1)
      
      tok26Tok27Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 26) 
          askTok27Name
          (fst testToken26)
          (snd testToken26)
          (fst testToken27)
          (snd testToken27)
          (unsafeRatio 1 1)

      tok27Tok28Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 27) 
          askTok28Name
          (fst testToken27)
          (snd testToken27)
          (fst testToken28)
          (snd testToken28)
          (unsafeRatio 1 1)

      tok28Tok29Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 28) 
          askTok29Name
          (fst testToken28)
          (snd testToken28)
          (fst testToken29)
          (snd testToken29)
          (unsafeRatio 1 1)
      
      tok29Tok30Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 29) 
          askTok30Name
          (fst testToken29)
          (snd testToken29)
          (fst testToken30)
          (snd testToken30)
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
          , [(askTok14Name,1)]
          , [(askTok15Name,1)]
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
          , MintBeacons [askTok14]
          , MintBeacons [askTok15]
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
          , ( Just tok13Tok14Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!13) askTok14Name 1
           <> (uncurry singleton testToken13) 10
            )
          , ( Just tok14Tok15Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!14) askTok15Name 1
           <> (uncurry singleton testToken14) 10
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
          , tok14MintRef
          , tok15MintRef
          ]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [ [(askTok16Name,1)]
          , [(askTok17Name,1)]
          , [(askTok18Name,1)]
          , [(askTok19Name,1)]
          , [(askTok20Name,1)]
          , [(askTok21Name,1)]
          , [(askTok22Name,1)]
          , [(askTok23Name,1)]
          , [(askTok24Name,1)]
          , [(askTok25Name,1)]
          , [(askTok26Name,1)]
          , [(askTok27Name,1)]
          , [(askTok28Name,1)]
          , [(askTok29Name,1)]
          , [(askTok30Name,1)]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons [askTok16]
          , MintBeacons [askTok17]
          , MintBeacons [askTok18]
          , MintBeacons [askTok19]
          , MintBeacons [askTok20]
          , MintBeacons [askTok21]
          , MintBeacons [askTok22]
          , MintBeacons [askTok23]
          , MintBeacons [askTok24]
          , MintBeacons [askTok25]
          , MintBeacons [askTok26]
          , MintBeacons [askTok27]
          , MintBeacons [askTok28]
          , MintBeacons [askTok29]
          , MintBeacons [askTok30]
          ]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just tok15Tok16Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!15) askTok16Name 1
           <> (uncurry singleton testToken15) 10
            )
          , ( Just tok16Tok17Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!16) askTok17Name 1
           <> (uncurry singleton testToken16) 10
            )
          , ( Just tok17Tok18Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!17) askTok18Name 1
           <> (uncurry singleton testToken17) 10
            )
          , ( Just tok18Tok19Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!18) askTok19Name 1
           <> (uncurry singleton testToken18) 10
            )
          , ( Just tok19Tok20Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!19) askTok20Name 1
           <> (uncurry singleton testToken19) 10
            )
          , ( Just tok20Tok21Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!20) askTok21Name 1
           <> (uncurry singleton testToken20) 10
            )
          , ( Just tok21Tok22Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!21) askTok22Name 1
           <> (uncurry singleton testToken21) 10
            )
          , ( Just tok22Tok23Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!22) askTok23Name 1
           <> (uncurry singleton testToken22) 10
            )
          , ( Just tok23Tok24Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!23) askTok24Name 1
           <> (uncurry singleton testToken23) 10
            )
          , ( Just tok24Tok25Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!24) askTok25Name 1
           <> (uncurry singleton testToken24) 10
            )
          , ( Just tok25Tok26Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!25) askTok26Name 1
           <> (uncurry singleton testToken25) 10
            )
          , ( Just tok26Tok27Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!26) askTok27Name 1
           <> (uncurry singleton testToken26) 10
            )
          , ( Just tok27Tok28Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!27) askTok28Name 1
           <> (uncurry singleton testToken27) 10
            )
          , ( Just tok28Tok29Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!28) askTok29Name 1
           <> (uncurry singleton testToken28) 10
            )
          , ( Just tok29Tok30Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!29) askTok30Name 1
           <> (uncurry singleton testToken29) 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = drop 15 ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = 
          [ tok15MintRef
          , tok16MintRef
          , tok17MintRef
          , tok18MintRef
          , tok19MintRef
          , tok20MintRef
          , tok21MintRef
          , tok22MintRef
          , tok23MintRef
          , tok24MintRef
          , tok25MintRef
          , tok26MintRef
          , tok27MintRef
          , tok28MintRef
          , tok29MintRef
          , tok30MintRef
          ]
      , createSwapRefAddress = refAddr
      }

  void $ waitNSlots 2

  spendRefScript <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  input1 <- txOutRefWithValue $ lovelaceValueOf 20_000_000
                             <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
  input2 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
                             <> uncurry singleton testToken1 10
  input3 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
                             <> uncurry singleton testToken2 10
  input4 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!3) askTok4Name 1
                             <> uncurry singleton testToken3 10
  input5 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!4) askTok5Name 1
                             <> uncurry singleton testToken4 10
  input6 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!5) askTok6Name 1
                             <> uncurry singleton testToken5 10
  input7 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!6) askTok7Name 1
                             <> uncurry singleton testToken6 10
  input8 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!7) askTok8Name 1
                             <> uncurry singleton testToken7 10
  input9 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!8) askTok9Name 1
                             <> uncurry singleton testToken8 10
  input10 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!9) askTok10Name 1
                             <> uncurry singleton testToken9 10
  input11 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!10) askTok11Name 1
                             <> uncurry singleton testToken10 10
  input12 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!11) askTok12Name 1
                             <> uncurry singleton testToken11 10
  input13 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!12) askTok13Name 1
                             <> uncurry singleton testToken12 10
  input14 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!13) askTok14Name 1
                             <> uncurry singleton testToken13 10
  input15 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!14) askTok15Name 1
                             <> uncurry singleton testToken14 10
  input16 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!15) askTok16Name 1
                             <> uncurry singleton testToken15 10
  input17 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!16) askTok17Name 1
                             <> uncurry singleton testToken16 10
  input18 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!17) askTok18Name 1
                             <> uncurry singleton testToken17 10
  input19 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!18) askTok19Name 1
                             <> uncurry singleton testToken18 10
  input20 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!19) askTok20Name 1
                             <> uncurry singleton testToken19 10
  input21 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!20) askTok21Name 1
                             <> uncurry singleton testToken20 10
  input22 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!21) askTok22Name 1
                             <> uncurry singleton testToken21 10
  input23 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!22) askTok23Name 1
                             <> uncurry singleton testToken22 10
  input24 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!23) askTok24Name 1
                             <> uncurry singleton testToken23 10
  input25 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!24) askTok25Name 1
                             <> uncurry singleton testToken24 10
  input26 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!25) askTok26Name 1
                             <> uncurry singleton testToken25 10
  input27 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!26) askTok27Name 1
                             <> uncurry singleton testToken26 10
  input28 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!27) askTok28Name 1
                             <> uncurry singleton testToken27 10
  input29 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!28) askTok29Name 1
                             <> uncurry singleton testToken28 10
  input30 <- txOutRefWithValue $ lovelaceValueOf 2_500_000
                             <> singleton (beaconCurrencySymbol $ ds!!29) askTok30Name 1
                             <> uncurry singleton testToken29 10
  
  callEndpoint @"close-or-update" h1 $
    CloseOrUpdateParams
      { closeOrUpdateBeaconsBurned = []
      , closeOrUpdateBeaconRedeemer = BurnBeacons
      , closeOrUpdateAddress = swapAddr
      , closeOrUpdateSpecificUTxOs = 
          [ input1
          -- , input2
          -- , input3
          -- , input4
          -- , input5
          -- , input6
          -- , input7
          -- , input8
          -- , input9
          -- , input10
          -- , input11
          -- , input12
          -- , input13
          -- , input14
          -- , input15
          -- , input16
          -- , input17
          -- , input18
          -- , input19
          -- , input20
          -- , input21
          -- , input22
          -- , input23
          -- , input24
          -- , input25
          -- , input26
          -- , input27
          -- , input28
          -- , input29
          -- , input30
          ]
      , closeOrUpdateNewSwaps = 
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          -- , ( Just tok1Tok2Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
          --  <> (uncurry singleton testToken1) 10
          --   )
          -- , ( Just tok2Tok3Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!2) askTok3Name 1
          --  <> (uncurry singleton testToken2) 10
          --   )
          -- , ( Just tok3Tok4Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!3) askTok4Name 1
          --  <> (uncurry singleton testToken3) 10
          --   )
          -- , ( Just tok4Tok5Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!4) askTok5Name 1
          --  <> (uncurry singleton testToken4) 10
          --   )
          -- , ( Just tok5Tok6Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!5) askTok6Name 1
          --  <> (uncurry singleton testToken5) 10
          --   )
          -- , ( Just tok6Tok7Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!6) askTok7Name 1
          --  <> (uncurry singleton testToken6) 10
          --   )
          -- , ( Just tok7Tok8Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!7) askTok8Name 1
          --  <> (uncurry singleton testToken7) 10
          --   )
          -- , ( Just tok8Tok9Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!8) askTok9Name 1
          --  <> (uncurry singleton testToken8) 10
          --   )
          -- , ( Just tok9Tok10Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!9) askTok10Name 1
          --  <> (uncurry singleton testToken9) 10
          --   )
          -- , ( Just tok10Tok11Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!10) askTok11Name 1
          --  <> (uncurry singleton testToken10) 10
          --   )
          -- , ( Just tok11Tok12Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!11) askTok12Name 1
          --  <> (uncurry singleton testToken11) 10
          --   )
          -- , ( Just tok12Tok13Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!12) askTok13Name 1
          --  <> (uncurry singleton testToken12) 10
          --   )
          -- , ( Just tok13Tok14Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!13) askTok14Name 1
          --  <> (uncurry singleton testToken13) 10
          --   )
          -- , ( Just tok14Tok15Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!14) askTok15Name 1
          --  <> (uncurry singleton testToken14) 10
          --   )
          -- , ( Just tok15Tok16Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!15) askTok16Name 1
          --  <> (uncurry singleton testToken15) 10
          --   )
          -- , ( Just tok16Tok17Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!16) askTok17Name 1
          --  <> (uncurry singleton testToken16) 10
          --   )
          ]
      , closeOrUpdateAsInline = True
      , closeOrUpdateScripts = ds
      , closeOrUpdateWithRefScripts = True
      , closeOrUpdateSpendRefScript = spendRefScript
      , closeOrUpdateMintRefScripts = []
      , closeOrUpdateRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: [DappScripts] -> TestTree
tests ds = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close/Update Swaps"
    [ checkPredicateOptions opts "Successfully close single swap"
        assertNoFailedTransactions (successfullyCloseSingleSwap ds)
    , checkPredicateOptions opts "Successfully update single swap"
        assertNoFailedTransactions (successfullyUpdateSingleSwap ds)
    , checkPredicateOptions opts "Successfully close multiple swaps for the same trading pair"
        assertNoFailedTransactions (successfullyCloseMultipleSwapsOfSamePair ds)
    , checkPredicateOptions opts "Successfully update multiple swaps for the same trading pair"
        assertNoFailedTransactions (successfullyUpdateMultipleSwapsOfSamePair ds)
    , checkPredicateOptions opts "Successfully mix updating and close for the same trading pair"
        assertNoFailedTransactions (successfullyMixUpdateAndCloseOfSamePair ds)
    , checkPredicateOptions opts "Successfully mix updating and close for different trading pairs"
        assertNoFailedTransactions (successfullyMixUpdateAndCloseOfDifferentPairs ds)
    , checkPredicateOptions opts "Fail if only beacon withdrawn during close"
        (Test.not assertNoFailedTransactions) (onlyBeaconWithdrawnDuringClose ds)
    , checkPredicateOptions opts "Fail if only beacon withdrawn during update"
        (Test.not assertNoFailedTransactions) (onlyBeaconWithdrawnDuringUpdate ds)
    , checkPredicateOptions opts "Fail if new price is negative"
        (Test.not assertNoFailedTransactions) (updateHasNegativePrice ds)
    , checkPredicateOptions opts "Fail if new price is zero"
        (Test.not assertNoFailedTransactions) (updateHasZeroPrice ds)
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) (ownerDidNotApprove ds)
    , checkPredicateOptions opts "Fail if beacons not stored with offer asset"
        (Test.not assertNoFailedTransactions) (updateNotStoredWithOfferAsset ds)
    , checkPredicateOptions opts "Fail if beacons not stored individually (same pair)"
        (Test.not assertNoFailedTransactions) (beaconsGroupedUpForSamePair ds)
    , checkPredicateOptions opts "Fail if beacons not stored individually (different pairs)"
        (Test.not assertNoFailedTransactions) (beaconsGroupedUpForDifferentPairs ds)
    , checkPredicateOptions opts "Fail if beacons mixed up during update"
        (Test.not assertNoFailedTransactions) (beaconsMixedUpDuringUpdate ds)
    , checkPredicateOptions opts "Successfully compose CloseOrUpdate with MintBeacons for independent utxos"
        assertNoFailedTransactions (successfullyComposeMintAndCloseOrUpdateWithIndependentUTxOs ds)
    , checkPredicateOptions opts "Successfully compose CloseOrUpdate with MintBeacons for dependent utxos"
        assertNoFailedTransactions (successfullyComposeMintAndCloseOrUpdateWithDependentUTxOs ds)
    , checkPredicateOptions opts "Fail if burned beacon not present in MintBeacons redeemer during composition"
        (Test.not assertNoFailedTransactions) (burnedBeaconNotPresentInRedeemer ds)
    ]

testTrace :: [DappScripts] -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . maxUpdateForDifferentOffers