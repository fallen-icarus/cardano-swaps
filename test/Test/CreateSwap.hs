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

module Test.CreateSwap
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
-- Create Swap Scenarios
-------------------------------------------------
successfullyCreateSingleSwap :: [DappScripts] -> EmulatorTrace ()
successfullyCreateSingleSwap ds = do
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

successfullyCreateMultipleSwapsOfSamePair :: [DappScripts] -> EmulatorTrace ()
successfullyCreateMultipleSwapsOfSamePair ds = do
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
          , ( Just adaTok1Datum{ swapPrice = unsafeRatio 1 1_000_000 }
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

successfullyCreateMultipleSwapsOfDifferentPairs :: [DappScripts] -> EmulatorTrace ()
successfullyCreateMultipleSwapsOfDifferentPairs ds = do
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
          (unsafeRatio 1 1_000_000)

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
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

differentAssetConfigInRedeemer :: [DappScripts] -> EmulatorTrace ()
differentAssetConfigInRedeemer ds = do
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
          (unsafeRatio 10 1_000_000)

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
          (unsafeRatio 1 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1),(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1,askTok3]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok2Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

mintOtherTokenInAdditionToBeacon :: [DappScripts] -> EmulatorTrace ()
mintOtherTokenInAdditionToBeacon ds = do
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
      { createSwapBeaconsMinted = [[(askTok1Name,1),("other",1)]]
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

beaconMintedToWrongAddress :: [DappScripts] -> EmulatorTrace ()
beaconMintedToWrongAddress ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ alwaysSucceedValidatorHash)
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

beaconMintedToNonStakingDappAddress :: [DappScripts] -> EmulatorTrace ()
beaconMintedToNonStakingDappAddress ds = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

  initializeRefScripts ds refAddr

  let swapAddr = Address (ScriptCredential $ spendingValidatorHash $ ds!!0)
                         Nothing

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

beaconsGroupedUp :: [DappScripts] -> EmulatorTrace ()
beaconsGroupedUp ds = do
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
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 2
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

extraBeaconWithdrawn :: [DappScripts] -> EmulatorTrace ()
extraBeaconWithdrawn ds = do
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
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

beaconNotStoredWithOfferAsset :: [DappScripts] -> EmulatorTrace ()
beaconNotStoredWithOfferAsset ds = do
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
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = tail ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [tok1MintRef]
      , createSwapRefAddress = refAddr
      }

datumHasWrongBeaconId :: [DappScripts] -> EmulatorTrace ()
datumHasWrongBeaconId ds = do
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
          (beaconCurrencySymbol $ ds !! 1) 
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

datumHasWrongBeaconName :: [DappScripts] -> EmulatorTrace ()
datumHasWrongBeaconName ds = do
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
          "askTok1Name"
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

datumHasWrongOfferId :: [DappScripts] -> EmulatorTrace ()
datumHasWrongOfferId ds = do
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
          (fst testToken1)
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

datumHasWrongOfferName :: [DappScripts] -> EmulatorTrace ()
datumHasWrongOfferName ds = do
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
          "other"
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

datumHasWrongAskId :: [DappScripts] -> EmulatorTrace ()
datumHasWrongAskId ds = do
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
          ""
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

datumHasWrongAskName :: [DappScripts] -> EmulatorTrace ()
datumHasWrongAskName ds = do
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
          ""
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

datumHasZeroPrice :: [DappScripts] -> EmulatorTrace ()
datumHasZeroPrice ds = do
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
          (unsafeRatio 0 1_000_000)

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

datumHasNegativePrice :: [DappScripts] -> EmulatorTrace ()
datumHasNegativePrice ds = do
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
          (unsafeRatio (-10) 1_000_000)

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

datumNotInline :: [DappScripts] -> EmulatorTrace ()
datumNotInline ds = do
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
      , createSwapAsInline = False
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

mixUpBeaconsWhenCreatingSwapsForDifferentPairs :: [DappScripts] -> EmulatorTrace ()
mixUpBeaconsWhenCreatingSwapsForDifferentPairs ds = do
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
          (unsafeRatio 1 1_000_000)

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok1Name,1),(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1,askTok2]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok2Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
          , ( Just adaTok1Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

maxCreationForSinglePair :: [DappScripts] -> EmulatorTrace ()
maxCreationForSinglePair ds = do
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
      { createSwapBeaconsMinted = [[(askTok1Name,59)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok1]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          replicate 59
            ( Just adaTok1Datum
            , lovelaceValueOf 5_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok1Name 1
            )
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

maxCreationForMultiplePairs :: [DappScripts] -> EmulatorTrace ()
maxCreationForMultiplePairs ds = do
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
            -- , (askTok31Name,1)
            -- , (askTok32Name,1)
            -- , (askTok33Name,1)
            -- , (askTok34Name,1)
            -- , (askTok35Name,1)
            -- , (askTok36Name,1)
            -- , (askTok37Name,1)
            -- , (askTok38Name,1)
            -- , (askTok39Name,1)
            -- , (askTok40Name,1)
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
              -- , askTok31
              -- , askTok32
              -- , askTok33
              -- , askTok34
              -- , askTok35
              -- , askTok36
              -- , askTok37
              -- , askTok38
              -- , askTok39
              -- , askTok40
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
          -- , ( Just adaTok31Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok31Name 1
          --   )
          -- , ( Just adaTok32Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok32Name 1
          --   )
          -- , ( Just adaTok33Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok33Name 1
          --   )
          -- , ( Just adaTok34Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok34Name 1
          --   )
          -- , ( Just adaTok35Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok35Name 1
          --   )
          -- , ( Just adaTok36Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok36Name 1
          --   )
          -- , ( Just adaTok37Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok37Name 1
          --   )
          -- , ( Just adaTok38Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok38Name 1
          --   )
          -- , ( Just adaTok39Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok39Name 1
          --   )
          -- , ( Just adaTok40Datum
          --   , lovelaceValueOf 5_000_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!0) askTok40Name 1
          --   )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef]
      , createSwapRefAddress = refAddr
      }

successfullyCreateSwapsOfDifferentOffers :: [DappScripts] -> EmulatorTrace ()
successfullyCreateSwapsOfDifferentOffers ds = do
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

  let askTok2 = uncurry AssetConfig testToken2
      askTok2Name = genBeaconName askTok2

      adaTok2Datum = 
        SwapDatum 
          (beaconCurrencySymbol $ ds !! 0) 
          askTok2Name
          ""
          ""
          (fst testToken2)
          (snd testToken2)
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

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = [[(askTok2Name,1)],[(askTok2Name,1)]]
      , createSwapBeaconRedeemers = [MintBeacons [askTok2], MintBeacons [askTok2]]
      , createSwapAddress = swapAddr
      , createSwapUTxOs =
          [ ( Just adaTok2Datum
            , lovelaceValueOf 20_000_000 
           <> singleton (beaconCurrencySymbol $ ds!!0) askTok2Name 1
            )
          , ( Just tok1Tok2Datum
            , lovelaceValueOf 2_500_000 
           <> singleton (beaconCurrencySymbol $ ds!!1) askTok2Name 1
           <> uncurry singleton testToken1 10
            )
          ]
      , createSwapAsInline = True
      , createSwapScripts = ds
      , createSwapWithRefScripts = True
      , createSwapRefScripts = [adaMintRef,tok1MintRef]
      , createSwapRefAddress = refAddr
      }

maxCreateSwapsOfDifferentOffers :: [DappScripts] -> EmulatorTrace ()
maxCreateSwapsOfDifferentOffers ds = do
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

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapBeaconsMinted = 
          [ [(askTok1Name,1)]
          , [(askTok2Name,1)]
          , [(askTok3Name,1)]
          , [(askTok4Name,1)]
          , [(askTok5Name,1)]
          -- , [(askTok6Name,1)]
          -- , [(askTok7Name,1)]
          -- , [(askTok8Name,1)]
          -- , [(askTok9Name,1)]
          -- , [(askTok10Name,1)]
          -- , [(askTok11Name,1)]
          -- , [(askTok12Name,1)]
          -- , [(askTok13Name,1)]
          -- , [(askTok14Name,1)]
          -- , [(askTok15Name,1)]
          -- , [(askTok16Name,1)]
          -- , [(askTok17Name,1)]
          -- , [(askTok18Name,1)]
          -- , [(askTok19Name,1)]
          -- , [(askTok20Name,1)]
          ]
      , createSwapBeaconRedeemers = 
          [ MintBeacons [askTok1]
          , MintBeacons [askTok2]
          , MintBeacons [askTok3]
          , MintBeacons [askTok4]
          , MintBeacons [askTok5]
          -- , MintBeacons [askTok6]
          -- , MintBeacons [askTok7]
          -- , MintBeacons [askTok8]
          -- , MintBeacons [askTok9]
          -- , MintBeacons [askTok10]
          -- , MintBeacons [askTok11]
          -- , MintBeacons [askTok12]
          -- , MintBeacons [askTok13]
          -- , MintBeacons [askTok14]
          -- , MintBeacons [askTok15]
          -- , MintBeacons [askTok16]
          -- , MintBeacons [askTok17]
          -- , MintBeacons [askTok18]
          -- , MintBeacons [askTok19]
          -- , MintBeacons [askTok20]
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
          -- , ( Just tok17Tok18Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!17) askTok18Name 1
          --  <> (uncurry singleton testToken17) 10
          --   )
          -- , ( Just tok18Tok19Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!18) askTok19Name 1
          --  <> (uncurry singleton testToken18) 10
          --   )
          -- , ( Just tok19Tok20Datum
          --   , lovelaceValueOf 2_500_000 
          --  <> singleton (beaconCurrencySymbol $ ds!!19) askTok20Name 1
          --  <> (uncurry singleton testToken19) 10
          --   )
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
          -- , tok6MintRef
          -- , tok7MintRef
          -- , tok8MintRef
          -- , tok9MintRef
          -- , tok10MintRef
          -- , tok11MintRef
          -- , tok12MintRef
          -- , tok13MintRef
          -- , tok14MintRef
          -- , tok15MintRef
          -- , tok16MintRef
          -- , tok17MintRef
          -- , tok18MintRef
          -- , tok19MintRef
          -- , tok20MintRef
          ]
      , createSwapRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: [DappScripts] -> TestTree
tests ds = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create Swap(s)"
    [ checkPredicateOptions opts "Successfully create single swap"
        assertNoFailedTransactions (successfullyCreateSingleSwap ds)
    , checkPredicateOptions opts "Successfully create multiple swaps for the same trading pair"
        assertNoFailedTransactions (successfullyCreateMultipleSwapsOfSamePair ds)
    , checkPredicateOptions opts "Successfully create multiple swaps for different trading pairs"
        assertNoFailedTransactions (successfullyCreateMultipleSwapsOfDifferentPairs ds)
    , checkPredicateOptions opts "Fail if redeemer has different assets than beacons minted"
        (Test.not assertNoFailedTransactions) (differentAssetConfigInRedeemer ds)
    , checkPredicateOptions opts "Fail if other token minted in addition to beacon"
        (Test.not assertNoFailedTransactions) (mintOtherTokenInAdditionToBeacon ds)
    , checkPredicateOptions opts "Fail if beacon minted to non-dApp address"
        (Test.not assertNoFailedTransactions) (beaconMintedToWrongAddress ds)
    , checkPredicateOptions opts "Fail if beacon minted to a dApp address without staking"
        (Test.not assertNoFailedTransactions) (beaconMintedToNonStakingDappAddress ds)
    , checkPredicateOptions opts "Fail if beacons are not stored individually"
        (Test.not assertNoFailedTransactions) (beaconsGroupedUp ds)
    , checkPredicateOptions opts "Fail if extra minted beacon is withdrawn"
        (Test.not assertNoFailedTransactions) (extraBeaconWithdrawn ds)
    , checkPredicateOptions opts "Fail if beacon not stored with offer asset"
        (Test.not assertNoFailedTransactions) (beaconNotStoredWithOfferAsset ds)
    , checkPredicateOptions opts "Fail if SwapDatum has wrong beaconId"
        (Test.not assertNoFailedTransactions) (datumHasWrongBeaconId ds)
    , checkPredicateOptions opts "Fail if SwapDatum has wrong beaconName"
        (Test.not assertNoFailedTransactions) (datumHasWrongBeaconName ds)
    , checkPredicateOptions opts "Fail if SwapDatum has wrong offerId"
        (Test.not assertNoFailedTransactions) (datumHasWrongOfferId ds)
    , checkPredicateOptions opts "Fail if SwapDatum has wrong offerName"
        (Test.not assertNoFailedTransactions) (datumHasWrongOfferName ds)
    , checkPredicateOptions opts "Fail if SwapDatum has wrong askId"
        (Test.not assertNoFailedTransactions) (datumHasWrongAskId ds)
    , checkPredicateOptions opts "Fail if SwapDatum has wrong askName"
        (Test.not assertNoFailedTransactions) (datumHasWrongAskName ds)
    , checkPredicateOptions opts "Fail if SwapDatum has zero price"
        (Test.not assertNoFailedTransactions) (datumHasZeroPrice ds)
    , checkPredicateOptions opts "Fail if SwapDatum has negative price"
        (Test.not assertNoFailedTransactions) (datumHasNegativePrice ds)
    , checkPredicateOptions opts "Fail if SwapDatum is not inline"
        (Test.not assertNoFailedTransactions) (datumNotInline ds)
    , checkPredicateOptions opts "Fail if SwapDatums mixed up when creating swaps for different pairs"
        (Test.not assertNoFailedTransactions) (mixUpBeaconsWhenCreatingSwapsForDifferentPairs ds)
    , checkPredicateOptions opts "Successfully create swaps of different offers"
        assertNoFailedTransactions (successfullyCreateSwapsOfDifferentOffers ds)
    ]

testTrace :: [DappScripts] -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . maxCreateSwapsOfDifferentOffers