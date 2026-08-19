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
  , genOfferBeaconName
  , genAskBeaconName

    -- * Datums
  , genSwapDatum  
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
  , offerId :: CurrencySymbol -- ^ The `CurrencySymbol` for the offer asset.
  , offerName :: TokenName -- ^ The `TokenName` for the offer asset.
  , offerBeacon :: TokenName -- ^ The offer beacon's `TokenName`.
  , askId :: CurrencySymbol -- ^ The `CurrencySymbol` for the ask asset.
  , askName :: TokenName -- ^ The `TokenName` for the ask asset.
  , askBeacon :: TokenName -- ^ The ask beacon's `TokenName`.
  , swapPrice :: PlutusRational -- ^ The price to take the offer asset as a fraction (Ask/Offer).
  , prevInput :: Maybe TxOutRef -- ^ The corresponding swap input's output reference.
  , expiration :: Maybe POSIXTime -- ^ The order's expiration.
  } deriving (Generic,Show,Eq)

instance ToJSON SwapDatum where
  toJSON SwapDatum{..} = 
    object [ "beacon_id" .= show beaconId
           , "pair_beacon" .= showTokenName pairBeacon
           , "offer_id" .= show offerId
           , "offer_name" .= showTokenName offerName
           , "offer_beacon" .= showTokenName offerBeacon
           , "ask_id" .= show askId
           , "ask_name" .= showTokenName askName
           , "ask_beacon" .= showTokenName askBeacon
           , "price" .= swapPrice 
           , "prev_input" .= prevInput
           , "expiration" .= expiration
           ]

data SwapRedeemer
  -- | Spend the swap as the owner using the beacon script as a minting policy.
  = SpendWithMint
  -- | Spend the swap as the owner using the beacon script as a staking validator. This is only
  -- for when no beacons need to be minted or burned in the transaction.
  | SpendWithStake
  -- | Take the offer asset and deposit the ask asset.
  | Swap
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
-- constructor tag in the Data encoding: append only, never reorder or remove constructors. This
-- type never appears in a datum or redeemer; it is internal to the beacon name derivation.
data SwapAsset
  -- | The offer asset. On-chain: @OfferAsset@ (Constr tag 121).
  = Offer CurrencySymbol TokenName
  -- | The ask asset. On-chain: @AskAsset@ (Constr tag 122).
  | Ask CurrencySymbol TokenName
  -- | The trading pair; the offer asset must come first. On-chain: @TradingPair@ (Constr tag 123).
  | TradingPair CurrencySymbol TokenName CurrencySymbol TokenName
  deriving (Generic,Show,Eq)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer
PlutusTx.unstableMakeIsData ''SwapAsset

-------------------------------------------------
-- Contracts
-------------------------------------------------
swapScript :: SerialisedScript
swapScript = parseScriptFromCBOR $ blueprints Map.! "one_way_swap.swap_script"

swapScriptSize :: Integer
swapScriptSize = getScriptSize swapScript

swapValidatorHash :: PV3.ValidatorHash
swapValidatorHash = PV3.ValidatorHash $ PV3.getScriptHash $ scriptHash swapScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "one_way_swap.beacon_script")
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

-- | Generate the beacon asset name by hashing the serialised `TradingPair`. The offer asset
-- always comes first, so the two directions of a pair get distinct beacon names even when ADA
-- (empty policy id and asset name) is part of the pair.
--
-- > sha2_256 ( serialiseData ( TradingPair offerId offerName askId askName ) )
genPairBeaconName :: OfferAsset -> AskAsset -> TokenName
genPairBeaconName (OfferAsset (offerId,offerName)) (AskAsset (askId,askName)) =
  hashSwapAsset $ TradingPair offerId offerName askId askName

-- | Generate the beacon asset name by hashing the serialised `Offer` asset.
--
-- > sha2_256 ( serialiseData ( Offer offerId offerName ) )
genOfferBeaconName :: OfferAsset -> TokenName
genOfferBeaconName (OfferAsset (sym,name)) = hashSwapAsset $ Offer sym name

-- | Generate the beacon asset name by hashing the serialised `Ask` asset.
--
-- > sha2_256 ( serialiseData ( Ask askId askName ) )
genAskBeaconName :: AskAsset -> TokenName
genAskBeaconName (AskAsset (sym,name)) = hashSwapAsset $ Ask sym name

-------------------------------------------------
-- Datums
-------------------------------------------------
genSwapDatum 
  :: OfferAsset 
  -> AskAsset 
  -> PlutusRational 
  -> Maybe TxOutRef 
  -> Maybe POSIXTime 
  -> SwapDatum
genSwapDatum o@(OfferAsset offerCfg) a@(AskAsset askCfg) price mPrev mExpir =
  SwapDatum
    { beaconId = beaconCurrencySymbol
    , pairBeacon = genPairBeaconName o a
    , offerId = fst offerCfg
    , offerName = snd offerCfg
    , offerBeacon = genOfferBeaconName o
    , askId = fst askCfg
    , askName = snd askCfg
    , askBeacon = genAskBeaconName a
    , swapPrice = price
    , prevInput = mPrev
    , expiration = mExpir
    }
