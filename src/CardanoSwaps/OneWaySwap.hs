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
  , swapValidatorHash
  , beaconScript
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
  , offerId :: CurrencySymbol -- ^ The `CurrencySymbol` for the offer asset.
  , offerName :: TokenName -- ^ The `TokenName` for the offer asset.
  , offerBeacon :: TokenName -- ^ The offer beacon's `TokenName`.
  , askId :: CurrencySymbol -- ^ The `CurrencySymbol` for the ask asset.
  , askName :: TokenName -- ^ The `TokenName` for the ask asset.
  , askBeacon :: TokenName -- ^ The ask beacon's `TokenName`.
  , swapPrice :: PlutusRational -- ^ Thw price to take the offer asset as a fraction (Ask/Offer).
  , prevInput :: Maybe TxOutRef -- ^ The corresponding swap input's output reference.
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
swapScript = parseScriptFromCBOR $ blueprints Map.! "one_way_swap.swap_script"

swapValidatorHash :: PV2.ValidatorHash
swapValidatorHash = PV2.ValidatorHash $ PV2.getScriptHash $ scriptHash swapScript

beaconScript :: SerialisedScript
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "one_way_swap.beacon_script")
    [PlutusTx.toData swapValidatorHash]

beaconCurrencySymbol :: PV2.CurrencySymbol
beaconCurrencySymbol = PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash beaconScript

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Generate the beacon asset name by hashing offer ++ ask. The policy id for
-- ADA is set to "00".
--
-- > sha2_256 ( offerId ++ offerName ++ askId ++ askName )
genPairBeaconName :: OfferAsset -> AskAsset -> TokenName
genPairBeaconName (OfferAsset assetX) (AskAsset assetY) =
  let ((CurrencySymbol sym1',TokenName name1),(CurrencySymbol sym2',TokenName name2)) =
        (assetX,assetY)
      sym1 = 
        if sym1' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym1'
      sym2 = 
        if sym2' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym2'
  in TokenName $ PlutusTx.sha2_256 $ sym1 <> name1 <> sym2 <> name2

-- | Generate the beacon asset name by hashing the offer asset policy id and name. 
--
-- > sha2_256 ( "01" ++ offerId ++ offerName )
genOfferBeaconName :: OfferAsset -> TokenName
genOfferBeaconName (OfferAsset (CurrencySymbol sym,TokenName name)) =
  TokenName $ PlutusTx.sha2_256 $ unsafeToBuiltinByteString "01" <> sym <> name

-- | Generate the beacon asset name by hashing the ask asset policy id and name.
--
-- > sha2_256 ( "02" ++ askId ++ askName )
genAskBeaconName :: AskAsset -> TokenName
genAskBeaconName (AskAsset (CurrencySymbol sym,TokenName name)) =
  TokenName $ PlutusTx.sha2_256 $ unsafeToBuiltinByteString "02" <> sym <> name

-------------------------------------------------
-- Datums
-------------------------------------------------
genSwapDatum :: OfferAsset -> AskAsset -> PlutusRational -> Maybe TxOutRef -> SwapDatum
genSwapDatum o@(OfferAsset offerCfg) a@(AskAsset askCfg) price mPrev =
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
    }
