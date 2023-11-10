{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 
 
All that is required to interface with the smart contracts is to add this module to your import
list:

@
import CardanoSwaps
@

This module re-exports the "CardanoSwaps.Utils" module, and renames the on-chain data types and
script functions to avoid name space collisions. If only one kind of swap is needed, you can get
started with just:

@
import CardanoSwaps.OneWaySwap
import CardanoSwaps.Utils
@

Importing more than just one kind of swap like this will result in name space collisions so the
"CardanoSwaps.OneWaySwap" module will need to be qualified. If you would like to work with the
blueprints directly, you can just use:

@
import CardanoSwaps.Blueprints
@

The "CardanoSwaps.Utils" module has types and functions that are helpful when interfacing with
the contracts.
-}

module CardanoSwaps
  (
    -- * One-Way Swap
    -- ** On-Chain Data Types
    OneWaySwapDatum(..)
  , OneWaySwapRedeemer(..)
  , OneWayBeaconRedeemer(..)

    -- ** Contracts
  , oneWaySwapScript
  , oneWaySwapValidator
  , oneWaySwapValidatorHash
  , oneWayBeaconScript
  , oneWayBeaconMintingPolicy
  , oneWayBeaconMintingPolicyHash
  , oneWayBeaconCurrencySymbol

    -- * Two-Way Swap
    -- ** On-Chain Data Types
  , TwoWaySwapDatum(..)
  , TwoWaySwapRedeemer(..)
  , TwoWayBeaconRedeemer(..)

    -- ** Contracts
  , twoWaySwapScript
  , twoWaySwapValidator
  , twoWaySwapValidatorHash
  , twoWayBeaconScript
  , twoWayBeaconMintingPolicy
  , twoWayBeaconMintingPolicyHash
  , twoWayBeaconCurrencySymbol

    -- * Helpers
  , getRequiredSwapDirection

    -- * Re-Export
  , module CardanoSwaps.Utils
  , OneWaySwap.genOneWayPairBeaconName
  , OneWaySwap.genOfferBeaconName
  , OneWaySwap.genAskBeaconName
  , TwoWaySwap.genTwoWayPairBeaconName
  , TwoWaySwap.genAssetBeaconName
  ) where

import Prelude hiding (fromInteger)
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import Plutus.Script.Utils.V2.Scripts
import Data.Aeson
import Data.List (sort)

import qualified CardanoSwaps.OneWaySwap as OneWaySwap
import qualified CardanoSwaps.TwoWaySwap as TwoWaySwap
import CardanoSwaps.Utils

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data OneWaySwapDatum = OneWaySwapDatum
  { oneWayBeaconId :: CurrencySymbol
  , oneWayPairBeacon :: TokenName
  , oneWayOfferId :: CurrencySymbol
  , oneWayOfferName :: TokenName
  , oneWayOfferBeacon :: TokenName
  , oneWayAskId :: CurrencySymbol
  , oneWayAskName :: TokenName
  , oneWayAskBeacon :: TokenName
  , oneWaySwapPrice :: PlutusRational
  , oneWayPrevInput :: Maybe TxOutRef
  }
  deriving (Generic,Show,Eq)

instance ToJSON OneWaySwapDatum where
  toJSON OneWaySwapDatum{..} = 
    object [ "beacon_id" .= show oneWayBeaconId
           , "pair_beacon" .= showTokenName oneWayPairBeacon
           , "offer_id" .= show oneWayOfferId
           , "offer_name" .= showTokenName oneWayOfferName
           , "offer_beacon" .= showTokenName oneWayOfferBeacon
           , "ask_id" .= show oneWayAskId
           , "ask_name" .= showTokenName oneWayAskName
           , "ask_beacon" .= showTokenName oneWayAskBeacon
           , "price" .= oneWaySwapPrice 
           , "prev_input" .= oneWayPrevInput
           ]

data OneWaySwapRedeemer
  = OneWayCloseOrUpdate
  | OneWaySwap
  deriving (Generic,Show)

data OneWayBeaconRedeemer
  = OneWayCreateSwap 
  | OneWayBurnBeacons
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''OneWaySwapDatum
PlutusTx.unstableMakeIsData ''OneWaySwapRedeemer
PlutusTx.unstableMakeIsData ''OneWayBeaconRedeemer

data TwoWaySwapDatum = TwoWaySwapDatum
  { twoWayBeaconId :: CurrencySymbol
  , twoWayPairBeacon :: TokenName
  , twoWayAsset1Id :: CurrencySymbol
  , twoWayAsset1Name :: TokenName
  , twoWayAsset1Beacon :: TokenName
  , twoWayAsset2Id :: CurrencySymbol
  , twoWayAsset2Name :: TokenName
  , twoWayAsset2Beacon :: TokenName
  , twoWayForwardPrice :: PlutusRational
  , twoWayReversePrice :: PlutusRational
  , twoWayPrevInput :: Maybe TxOutRef
  } deriving (Generic,Show,Eq)

instance ToJSON TwoWaySwapDatum where
  toJSON TwoWaySwapDatum{..} = 
    object [ "beacon_id" .= show twoWayBeaconId
           , "pair_beacon" .= showTokenName twoWayPairBeacon
           , "asset1_id" .= show twoWayAsset1Id
           , "asset1_name" .= showTokenName twoWayAsset1Name
           , "asset1_beacon" .= showTokenName twoWayAsset1Beacon
           , "asset2_id" .= show twoWayAsset2Id
           , "asset2_name" .= showTokenName twoWayAsset2Name
           , "asset2_beacon" .= showTokenName twoWayAsset2Beacon
           , "forward_price" .= twoWayForwardPrice 
           , "reverse_price" .= twoWayReversePrice 
           , "prev_input" .= twoWayPrevInput
           ]

data TwoWaySwapRedeemer
  = TwoWayCloseOrUpdate
  | TwoWayForwardSwap
  | TwoWayReverseSwap
  deriving (Generic,Show)

data TwoWayBeaconRedeemer
  = TwoWayCreateSwap 
  | TwoWayBurnBeacons
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''TwoWaySwapDatum
PlutusTx.unstableMakeIsData ''TwoWaySwapRedeemer
PlutusTx.unstableMakeIsData ''TwoWayBeaconRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
oneWaySwapScript :: Ledger.Script
oneWaySwapScript = OneWaySwap.swapScript

oneWaySwapValidator :: Validator
oneWaySwapValidator = Validator oneWaySwapScript

oneWaySwapValidatorHash :: ValidatorHash
oneWaySwapValidatorHash = validatorHash oneWaySwapValidator

oneWayBeaconScript :: Ledger.Script
oneWayBeaconScript = OneWaySwap.beaconScript

oneWayBeaconMintingPolicy :: MintingPolicy
oneWayBeaconMintingPolicy = MintingPolicy oneWayBeaconScript

oneWayBeaconMintingPolicyHash :: MintingPolicyHash
oneWayBeaconMintingPolicyHash = mintingPolicyHash oneWayBeaconMintingPolicy

oneWayBeaconCurrencySymbol :: CurrencySymbol
oneWayBeaconCurrencySymbol = scriptCurrencySymbol oneWayBeaconMintingPolicy

twoWaySwapScript :: Ledger.Script
twoWaySwapScript = TwoWaySwap.swapScript

twoWaySwapValidator :: Validator
twoWaySwapValidator = Validator twoWaySwapScript

twoWaySwapValidatorHash :: ValidatorHash
twoWaySwapValidatorHash = validatorHash twoWaySwapValidator

twoWayBeaconScript :: Ledger.Script
twoWayBeaconScript = TwoWaySwap.beaconScript

twoWayBeaconMintingPolicy :: MintingPolicy
twoWayBeaconMintingPolicy = MintingPolicy twoWayBeaconScript

twoWayBeaconMintingPolicyHash :: MintingPolicyHash
twoWayBeaconMintingPolicyHash = mintingPolicyHash twoWayBeaconMintingPolicy

twoWayBeaconCurrencySymbol :: CurrencySymbol
twoWayBeaconCurrencySymbol = scriptCurrencySymbol twoWayBeaconMintingPolicy

-------------------------------------------------
-- Helpers
-------------------------------------------------
-- | Get the required two-way swap redeemer based on the swap direction.
getRequiredSwapDirection :: OfferAsset -> AskAsset -> TwoWaySwapRedeemer
getRequiredSwapDirection (OfferAsset offer) (AskAsset ask)
  | offer == asset1 = TwoWayReverseSwap
  | otherwise = TwoWayForwardSwap
  where
    [asset1,_] = sort [offer,ask]
