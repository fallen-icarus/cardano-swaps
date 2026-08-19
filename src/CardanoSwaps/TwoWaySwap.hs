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
  , SwapAsset(..)

    -- * Contracts
  , swapScript
  , swapScriptSize
  , swapValidatorHash
  , beaconScript
  , beaconScriptSize
  , beaconCurrencySymbol

    -- * Beacon Names
  , genPairBeaconName
  , genAssetBeaconName

    -- * Datums
  , genSwapDatum

    -- * Helpers
  , getRequiredSwapDirection
  ) where

import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusTx.Builtins as Builtins
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Aeson
import qualified Plutus.Script.Utils.Scripts as PV3
import qualified PlutusLedgerApi.V3 as PV3

import CardanoSwaps.Utils
import CardanoSwaps.Blueprints

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol -- ^ `CurrencySymbol` for the `beaconScript`.
  , pairBeacon :: TokenName -- ^ The pair beacon's `TokenName` for this trading pair.
  , asset1Id :: CurrencySymbol -- ^ The `CurrencySymbol` for asset1.
  , asset1Name :: TokenName -- ^ The `TokenName` for asset1.
  , asset1Beacon :: TokenName -- ^ The asset beacon's `TokenName` for asset1.
  , asset2Id :: CurrencySymbol -- ^ The `CurrencySymbol` for asset2.
  , asset2Name :: TokenName -- ^ The `TokenName` for asset2.
  , asset2Beacon :: TokenName -- ^ The asset beacon's `TokenName` for asset2.
  , asset1Price :: PlutusRational -- ^ The price to take asset1 as a fraction (Asset2/Asset1).
  , asset2Price :: PlutusRational -- ^ The price to take asset2 as a fraction (Asset1/Asset2).
  , prevInput :: Maybe TxOutRef -- ^ The corresponding swap input's output reference.
  , expiration :: Maybe POSIXTime -- ^ The order's expiration.
  } deriving (Generic,Show,Eq)

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
           , "asset1_price" .= asset1Price 
           , "asset2_price" .= asset2Price 
           , "prev_input" .= prevInput
           , "expiration" .= expiration
           ]

data SwapRedeemer
  -- | Spend the swap as the owner using the beacon script as a minting policy.
  = SpendWithMint 
  -- | Spend the swap as the owner using the beacon script as a staking validator. This is only
  -- for when no beacons need to be minted or burned in the transaction.
  | SpendWithStake 
  -- | Take asset1 and deposit asset2.
  | TakeAsset1 
  -- | Take asset2 and deposit asset1.
  | TakeAsset2 
  deriving (Generic,Show)

data BeaconRedeemer
  -- | Register the beacon script for staking execution. This can only be done once. The credential
  -- connat be de-registered or delegated.
  = RegisterBeaconScript
  -- | Execute the beacon script as a minting policy. Used anytime beacons must be minted or burned.
  | CreateOrCloseSwaps 
  -- | Execute the beacon script as a staking validtor. Used anytime beacons do not need to be
  -- minted or burned.
  | UpdateSwaps
  deriving (Generic,Show)

-- | The pre-image for beacon token names. Every beacon name is:
--
-- > sha2_256 ( serialiseData ( toBuiltinData swapAsset ) )
--
-- The Data encoding tags each constructor and length-prefixes each field, so the encoding is
-- injective: distinct assets/pairs can never produce the same pre-hash bytes, and beacon names
-- cannot collide without a sha2_256 collision.
--
-- The constructor order must match the on-chain SwapAsset type exactly since it determines the
-- constructor tag in the Data encoding: append only, never reorder or remove constructors. The
-- tags may coincide with the one-way swap's SwapAsset tags; that is harmless because the two
-- protocols use different beacon policies. This type never appears in a datum or redeemer; it is
-- internal to the beacon name derivation.
data SwapAsset
  -- | An asset in the pair. On-chain: @Asset@ (Constr tag 121).
  = Asset CurrencySymbol TokenName
  -- | The trading pair, already sorted: asset1 < asset2 lexicographically. On-chain:
  -- @SortedPair@ (Constr tag 122).
  | SortedPair CurrencySymbol TokenName CurrencySymbol TokenName
  deriving (Generic,Show,Eq)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer
PlutusTx.unstableMakeIsData ''SwapAsset

-------------------------------------------------
-- Contracts
-------------------------------------------------
swapScript :: SerialisedScript
swapScript = parseScriptFromCBOR $ blueprints Map.! "two_way_swap.swap_script"

swapScriptSize :: Integer
swapScriptSize = getScriptSize swapScript

swapValidatorHash :: PV3.ValidatorHash
swapValidatorHash = PV3.ValidatorHash $ PV3.getScriptHash $ scriptHash swapScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "two_way_swap.beacon_script")
    [PlutusTx.toData swapValidatorHash]

beaconScriptSize :: Integer
beaconScriptSize = getScriptSize beaconScript

beaconCurrencySymbol :: PV3.CurrencySymbol
beaconCurrencySymbol = PV3.CurrencySymbol $ PV3.getScriptHash $ scriptHash beaconScript

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Hash the Data encoding of a `SwapAsset` to create a beacon name.
hashSwapAsset :: SwapAsset -> TokenName
hashSwapAsset = TokenName . PlutusTx.sha2_256 . Builtins.serialiseData . PlutusTx.toBuiltinData

-- | Generate the beacon asset name by hashing the serialised `SortedPair`. The trading pair is
-- first sorted so that the beacon name is independent of the ordering. ADA (empty policy id and
-- asset name) needs no special handling since the Data encoding length-prefixes every field.
--
-- > sha2_256 ( serialiseData ( SortedPair asset1Id asset1Name asset2Id asset2Name ) )
genPairBeaconName :: AssetConfig -> AssetConfig -> TokenName
genPairBeaconName assetX assetY =
  let ((sym1,name1),(sym2,name2)) =
       if assetY < assetX then (assetY,assetX) else (assetX,assetY)
  in hashSwapAsset $ SortedPair sym1 name1 sym2 name2

-- | Generate the beacon asset name by hashing the serialised `Asset`.
--
-- > sha2_256 ( serialiseData ( Asset assetId assetName ) )
genAssetBeaconName :: AssetConfig -> TokenName
genAssetBeaconName (sym,name) = hashSwapAsset $ Asset sym name

-------------------------------------------------
-- Datums
-------------------------------------------------
-- | Create the datum for a swap. The format should be:
-- 
-- > genSwapDatum (firstAsset,secondAsset) firstPrice secondPrice mPrev
--
-- Which asset is first or second does not matter; just make sure the
-- first price corresponds to __taking__ the first asset and the second price
-- corresponds to __taking__ the second asset.
genSwapDatum 
  :: TwoWayPair 
  -> PlutusRational 
  -> PlutusRational 
  -> Maybe TxOutRef 
  -> Maybe POSIXTime
  -> SwapDatum
genSwapDatum (firstAsset,secondAsset) firstPrice secondPrice mPrev mExpir =
    let (asset1,asset2) = 
          if firstAsset < secondAsset 
          then (firstAsset,secondAsset) 
          else (secondAsset,firstAsset)
        (asset1Price,asset2Price) = 
          if asset1 == firstAsset 
          then (firstPrice,secondPrice)
          else (secondPrice,firstPrice)
    in SwapDatum 
        { beaconId = beaconCurrencySymbol
        , pairBeacon = genPairBeaconName asset1 asset2
        , asset1Id = fst asset1
        , asset1Name = snd asset1
        , asset1Beacon = genAssetBeaconName asset1
        , asset2Id = fst asset2
        , asset2Name = snd asset2
        , asset2Beacon = genAssetBeaconName asset2
        , asset1Price = asset1Price
        , asset2Price = asset2Price
        , prevInput = mPrev
        , expiration = mExpir
        }

-------------------------------------------------
-- Helpers
-------------------------------------------------
-- | Get the required two-way swap redeemer based on the desired swap direction.
getRequiredSwapDirection :: OfferAsset -> AskAsset -> SwapRedeemer
getRequiredSwapDirection (OfferAsset offer) (AskAsset ask)
  | offer == min offer ask = TakeAsset1
  | otherwise = TakeAsset2
