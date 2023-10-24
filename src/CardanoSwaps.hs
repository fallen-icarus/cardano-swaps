{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
    -- * One Way Swap
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

    -- * Re-Export
  , module CardanoSwaps.Utils
  ) where

import Prelude hiding (fromInteger)
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import Plutus.Script.Utils.V2.Scripts

import qualified CardanoSwaps.OneWaySwap as OneWaySwap
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
  , oneWaySwapPrice :: PlutusRational
  , oneWayPrevInput :: Maybe TxOutRef
  }
  deriving (Generic,Show,Eq)

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
