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
  , swapValidatorHash
  , beaconScript
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
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Aeson
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2

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
  -- | Execute the beacon script as a minting policy. Used anytime beacons must be minted or burned.
  = CreateOrCloseSwaps 
  -- | Execute the beacon script as a staking validtor. Used anytime beacons do not need to be
  -- minted or burned.
  | UpdateSwaps
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
swapScript :: SerialisedScript
swapScript = parseScriptFromCBOR $ blueprints Map.! "two_way_swap.swap_script"

swapValidatorHash :: PV2.ValidatorHash
swapValidatorHash = PV2.ValidatorHash $ PV2.getScriptHash $ scriptHash swapScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "two_way_swap.beacon_script")
    [PlutusTx.toData swapValidatorHash]

beaconCurrencySymbol :: PV2.CurrencySymbol
beaconCurrencySymbol = PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash beaconScript

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Generate the beacon asset name by hashing asset1 ++ asset2. The trading pair is first
-- sorted so that the beacon name is independent of the ordering. The policy id for
-- ADA is set to "00" __after__ sorting.
--
-- > sha2_256 ( asset1Id ++ asset1Name ++ asset2Id ++ asset2Name )
genPairBeaconName :: AssetConfig -> AssetConfig -> TokenName
genPairBeaconName assetX assetY =
  let ((CurrencySymbol sym1',TokenName name1),(CurrencySymbol sym2',TokenName name2)) =
       if assetY < assetX then (assetY,assetX) else (assetX,assetY) 
      sym1 = 
        if sym1' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym1'
      sym2 = 
        if sym2' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym2'
  in TokenName $ PlutusTx.sha2_256 $ sym1 <> name1 <> sym2 <> name2

-- | Generate the beacon asset name by hashing the asset policy id and name. 
--
-- > sha2_256 ( assetId ++ assetName )
genAssetBeaconName :: AssetConfig -> TokenName
genAssetBeaconName (CurrencySymbol sym,TokenName name) =
  TokenName $ PlutusTx.sha2_256 $ sym <> name

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
genSwapDatum :: TwoWayPair -> PlutusRational -> PlutusRational -> Maybe TxOutRef -> SwapDatum
genSwapDatum (firstAsset,secondAsset) firstPrice secondPrice mPrev =
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
        }

-------------------------------------------------
-- Helpers
-------------------------------------------------
-- | Get the required two-way swap redeemer based on the desired swap direction.
getRequiredSwapDirection :: OfferAsset -> AskAsset -> SwapRedeemer
getRequiredSwapDirection (OfferAsset offer) (AskAsset ask)
  | offer == min offer ask = TakeAsset1
  | otherwise = TakeAsset2
