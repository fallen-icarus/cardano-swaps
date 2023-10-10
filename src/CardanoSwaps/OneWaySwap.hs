{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

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

    -- * Generate Beacon Name
  , genBeaconName
  ) where

import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import qualified Data.Map as Map
import Plutus.Script.Utils.V2.Scripts
import qualified PlutusTx.Prelude as Plutus

import CardanoSwaps.Utils
import CardanoSwaps.Blueprints

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol
  , beaconName :: TokenName
  , offerId :: CurrencySymbol
  , offerName :: TokenName
  , askId :: CurrencySymbol
  , askName :: TokenName
  , swapPrice :: PlutusRational
  }
  deriving (Generic,Show,Eq)

data SwapRedeemer
  = CloseOrUpdate
  | Swap
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateSwap [AssetConfig] -- ^ The assets being asked for.
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

-- | This script is still missing the `AssetConfig`.
partialBeaconScript :: Ledger.Script
partialBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "one_way_swap.mint")
    [toData swapValidatorHash]

beaconScript :: AssetConfig -> Ledger.Script
beaconScript cfg = applyArguments partialBeaconScript [toData cfg]

beaconMintingPolicy :: AssetConfig -> MintingPolicy
beaconMintingPolicy = MintingPolicy . beaconScript

beaconMintingPolicyHash :: AssetConfig -> MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash . beaconMintingPolicy

beaconCurrencySymbol :: AssetConfig -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconMintingPolicy

-------------------------------------------------
-- Generate Beacon Name
-------------------------------------------------
-- | Generate the beacon asset name by hashing the ask asset policy id and name.
genBeaconName :: AssetConfig -> TokenName
genBeaconName ((CurrencySymbol sym),(TokenName name)) =
  TokenName $ Plutus.sha2_256 $ sym <> name
