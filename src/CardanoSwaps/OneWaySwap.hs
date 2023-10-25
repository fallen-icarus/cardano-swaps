{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CardanoSwaps.OneWaySwap
  (
    -- * On-Chain Data Types
    SwapDatum(..)
  , SwapRedeemer(..)
  , BeaconRedeemer(..)

    -- * Contracts
  , swapScript
  , swapValidator
  , swapValidatorHash
  , beaconScript
  , beaconMintingPolicy
  , beaconMintingPolicyHash
  , beaconCurrencySymbol
  ) where

import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import qualified Data.Map as Map
import Plutus.Script.Utils.V2.Scripts
import Data.Aeson

import CardanoSwaps.Utils
import CardanoSwaps.Blueprints

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol
  , pairBeacon :: TokenName
  , offerId :: CurrencySymbol
  , offerName :: TokenName
  , offerBeacon :: TokenName
  , askId :: CurrencySymbol
  , askName :: TokenName
  , swapPrice :: PlutusRational
  , prevInput :: Maybe TxOutRef
  }
  deriving (Generic,Show,Eq)

instance ToJSON SwapDatum where
  toJSON SwapDatum{..} = 
    object [ "beacon_id" .= show beaconId
           , "pair_beacon" .= showTokenName pairBeacon
           , "offer_id" .= show offerId
           , "offer_name" .= showTokenName offerName
           , "offer_beacon" .= showTokenName offerBeacon
           , "ask_id" .= show askId
           , "ask_name" .= showTokenName askName
           , "price" .= swapPrice 
           , "prev_input" .= prevInput
           ]

data SwapRedeemer
  = CloseOrUpdate
  | Swap
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateSwap
  | BurnBeacons
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
swapScript :: Ledger.Script
swapScript = parseScriptFromCBOR $ blueprints Map.! "one_way_swap.spend"

swapValidator :: Validator
swapValidator = Validator swapScript

swapValidatorHash :: ValidatorHash
swapValidatorHash = validatorHash swapValidator

beaconScript :: Ledger.Script
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "one_way_swap.mint")
    [toData swapValidatorHash]

beaconMintingPolicy :: MintingPolicy
beaconMintingPolicy = MintingPolicy beaconScript

beaconMintingPolicyHash :: MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash beaconMintingPolicy

beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol beaconMintingPolicy
