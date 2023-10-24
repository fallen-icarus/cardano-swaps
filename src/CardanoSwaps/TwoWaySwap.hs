{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module CardanoSwaps.TwoWaySwap
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
import qualified PlutusTx.Prelude as Plutus
import Data.Maybe (fromJust)

import CardanoSwaps.Utils
import CardanoSwaps.Blueprints

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol
  , pairBeacon :: TokenName
  , asset1Id :: CurrencySymbol
  , asset1Name :: TokenName
  , asset1Beacon :: TokenName
  , asset2Id :: CurrencySymbol
  , asset2Name :: TokenName
  , asset2Beacon :: TokenName
  , forwardPrice :: PlutusRational
  , reversePrice :: PlutusRational
  , prevInput :: Maybe TxOutRef
  }
  deriving (Generic,Show,Eq)

data SwapRedeemer
  = CloseOrUpdate
  | ForwardSwap
  | ReverseSwap
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
swapScript = parseScriptFromCBOR $ blueprints Map.! "two_way_swap.spend"

swapValidator :: Validator
swapValidator = Validator swapScript

swapValidatorHash :: ValidatorHash
swapValidatorHash = validatorHash swapValidator

beaconScript :: Ledger.Script
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "two_way_swap.mint")
    [toData swapValidatorHash]

beaconMintingPolicy :: MintingPolicy
beaconMintingPolicy = MintingPolicy beaconScript

beaconMintingPolicyHash :: MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash beaconMintingPolicy

beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol beaconMintingPolicy

