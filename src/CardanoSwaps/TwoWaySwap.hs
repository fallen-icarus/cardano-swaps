{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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

    -- * Beacon Names
  , genTwoWayPairBeaconName
  , genAssetBeaconName

    -- * Helpers
  , getRequiredSwapDirection
  ) where

import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx.Prelude as Plutus
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import qualified Data.Map as Map
import Plutus.Script.Utils.V2.Scripts
import Data.Aeson
import Data.List (sort)

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

instance ToJSON SwapDatum where
  toJSON SwapDatum{..} = 
    object [ "beacon_id" .= show beaconId
           , "pair_beacon" .= showTokenName pairBeacon
           , "asset1_id" .= show asset1Id
           , "asset1_name" .= showTokenName asset1Name
           , "asset1_beacon" .= showTokenName asset1Beacon
           , "asset2_id" .= show asset2Id
           , "asset2_name" .= showTokenName asset2Name
           , "asset2_beacon" .= showTokenName asset2Beacon
           , "forward_price" .= forwardPrice 
           , "reverse_price" .= reversePrice 
           , "prev_input" .= prevInput
           ]

data SwapRedeemer
  = SpendWithMint
  | SpendWithStake
  | ForwardSwap
  | ReverseSwap
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateOrCloseSwaps 
  | UpdateSwaps
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
swapScript :: Ledger.Script
swapScript = parseScriptFromCBOR $ blueprints Map.! "two_way_swap.swap_script"

swapValidator :: Validator
swapValidator = Validator swapScript

swapValidatorHash :: ValidatorHash
swapValidatorHash = validatorHash swapValidator

beaconScript :: Ledger.Script
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "two_way_swap.beacon_script")
    [toData swapValidatorHash]

beaconMintingPolicy :: MintingPolicy
beaconMintingPolicy = MintingPolicy beaconScript

beaconMintingPolicyHash :: MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash beaconMintingPolicy

beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol beaconMintingPolicy

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Generate the beacon asset name by hashing asset1 ++ asset2. The trading pair is first
-- sorted so that the beacon name is independent of the ordering. This is used for two-way swaps.
genTwoWayPairBeaconName :: AssetConfig -> AssetConfig -> TokenName
genTwoWayPairBeaconName assetX assetY =
  let (((CurrencySymbol sym1'),(TokenName name1)),((CurrencySymbol sym2'),(TokenName name2))) =
       if assetY < assetX then (assetY,assetX) else (assetX,assetY) 
      sym1 = 
        if sym1' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym1'
      sym2 = 
        if sym2' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym2'
  in TokenName $ Plutus.sha2_256 $ sym1 <> name1 <> sym2 <> name2

-- | Generate the beacon asset name by hashing the ask asset policy id and name.
genAssetBeaconName :: AssetConfig -> TokenName
genAssetBeaconName ((CurrencySymbol sym),(TokenName name)) =
  TokenName $ Plutus.sha2_256 $ sym <> name

-------------------------------------------------
-- Helpers
-------------------------------------------------
-- | Get the required two-way swap redeemer based on the desired swap direction.
getRequiredSwapDirection :: OfferAsset -> AskAsset -> SwapRedeemer
getRequiredSwapDirection (OfferAsset offer) (AskAsset ask)
  | offer == asset1 = ReverseSwap
  | otherwise = ForwardSwap
  where
    [asset1,_] = sort [offer,ask]
