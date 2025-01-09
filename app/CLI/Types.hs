{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CLI.Types where

import Data.Aeson
import Data.Text (Text)
import Prettyprinter
import Control.Monad (mzero)

import CardanoSwaps.Utils
import qualified CardanoSwaps.OneWaySwap as OneWay
import qualified CardanoSwaps.TwoWaySwap as TwoWay

data Command
  = ExportScript Script FilePath
  | CreateDatum InternalDatum FilePath
  | CreateSpendingRedeemer SpendingRedeemer FilePath
  | CreateMintingRedeemer MintingRedeemer FilePath
  | BeaconInfo BeaconInfo Output
  | Query Query
  | Submit Network Endpoint FilePath
  | EvaluateTx Network Endpoint FilePath

data Script 
  = OneWayBeaconScript
  | OneWaySwapScript
  | TwoWayBeaconScript 
  | TwoWaySwapScript

-- | This has all the info necessary to create the actual SwapDatums.
data InternalDatum 
  = InternalOneWaySwapDatum 
      OfferAsset
      AskAsset
      PlutusRational -- ^ Swap price
      (Maybe TxOutRef)
  | InternalTwoWaySwapDatum
      TwoWayPair
      PlutusRational -- ^ ForwardSwap price.
      PlutusRational -- ^ reverseSwap price.
      (Maybe TxOutRef)

data SpendingRedeemer
  = OneWaySpendingRedeemer OneWay.SwapRedeemer
  | TwoWaySpendingRedeemer InternalTwoWaySwapRedeemer

data InternalTwoWaySwapRedeemer
  = KnownTwoWaySwapRedeemer TwoWay.SwapRedeemer
  | UnknownTwoWaySwapRedeemer OfferAsset AskAsset 
      -- ^ For when the swap direction is unknown (eg, ForwardSwap vs ReverseSwap)

data MintingRedeemer
  = OneWayMintingRedeemer OneWay.BeaconRedeemer
  | TwoWayMintingRedeemer TwoWay.BeaconRedeemer

data BeaconInfo 
  = OneWayPolicyId
  | OneWayOfferBeaconName OfferAsset
  | OneWayAskBeaconName AskAsset
  | OneWayPairBeaconName (OfferAsset,AskAsset)
  | TwoWayPolicyId
  | TwoWayAssetBeaconName AssetConfig
  | TwoWayPairBeaconName TwoWayPair

-- | For when saving to file is optional
data Output = Stdout | File FilePath

data Network
  = PreProdTestnet
  | Mainnet

data Endpoint
  = Koios

newtype TxCBOR = TxCBOR Text

instance FromJSON TxCBOR where
  parseJSON (Object o) = TxCBOR <$> o .: "cborHex"
  parseJSON _ = mzero

data Format = JSON | Pretty | Plain

newtype UserAddress = UserAddress Text 
  deriving (Show,Eq)

instance Pretty UserAddress where
  pretty (UserAddress addr) = pretty addr

data Query
  = QueryOwnSwaps QueryOwnSwaps
  | QueryAllSwaps QueryAll
  | QueryPersonal Network Endpoint UserAddress Format Output
  -- | Query the current protocol parameters.
  | QueryParameters Network Output

data QueryOwnSwaps
  = QueryOwnOneWaySwaps Network Endpoint UserAddress Format Output
  | QueryOwnOneWaySwapsByOffer Network Endpoint UserAddress OfferAsset Format Output
  | QueryOwnOneWaySwapsByAsk Network Endpoint UserAddress AskAsset Format Output
  | QueryOwnOneWaySwapsByTradingPair Network Endpoint UserAddress OfferAsset AskAsset Format Output
  | QueryOwnTwoWaySwaps Network Endpoint UserAddress Format Output
  | QueryOwnTwoWaySwapsByOffer Network Endpoint UserAddress AssetConfig Format Output
  | QueryOwnTwoWaySwapsByAsk Network Endpoint UserAddress AssetConfig Format Output
  | QueryOwnTwoWaySwapsByTradingPair Network Endpoint UserAddress TwoWayPair Format Output

data QueryAll
  = QueryAllSwapsByOffer Network Endpoint OfferAsset Format Output
  | QueryAllSwapsByAsk Network Endpoint AskAsset Format Output
  | QueryAllSwapsByTradingPair Network Endpoint OfferAsset AskAsset Format Output

data Asset = Asset
  { assetPolicyId :: Text
  , assetTokenName :: Text
  , assetQuantity :: Text
  } deriving (Show,Eq)

instance Pretty Asset where
  pretty Asset{..} =
    if assetPolicyId == ""
    then pretty assetQuantity <+> pretty @Text "lovelace"
    else pretty assetQuantity <+> pretty (assetPolicyId <> "." <> assetTokenName)

instance ToJSON Asset where
  toJSON Asset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" || assetPolicyId == ""
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]

data PersonalUTxO = PersonalUTxO
  { personalTxHash :: Text
  , personalOutputIndex :: Integer
  , personalValue :: [Asset]
  , personalDatumHash :: Maybe Text
  , personalReferenceScriptHash :: Maybe Text
  } deriving (Show,Eq)

instance Ord PersonalUTxO where
  (PersonalUTxO hash1 _ _ _ _) <= (PersonalUTxO hash2 _ _ _ _) = hash1 <= hash2

instance ToJSON PersonalUTxO where
  toJSON PersonalUTxO{..} =
    object [ "tx_hash" .= personalTxHash
           , "output_index" .= personalOutputIndex
           , "reference_script_hash" .= personalReferenceScriptHash
           , "datum_hash" .= personalDatumHash
           , "assets" .= personalValue
           ]

data SwapDatum
  = OneWayDatum OneWay.SwapDatum
  | TwoWayDatum TwoWay.SwapDatum
  deriving (Show)

instance ToJSON SwapDatum where
  toJSON (OneWayDatum datum) =
    object [ "type" .= ("one-way" :: Text)
           , "datum" .= datum
           ]
  toJSON (TwoWayDatum datum) =
    object [ "type" .= ("two-way" :: Text)
           , "datum" .= datum
           ]

-- | Type that captures all info a user needs to interact with available swaps.
data SwapUTxO = SwapUTxO
  { swapAddress :: UserAddress
  , swapTxHash :: Text
  , swapOutputIndex :: Integer
  , swapValue :: [Asset]
  , swapDatum :: Maybe SwapDatum
  } deriving (Show)

instance ToJSON SwapUTxO where
  toJSON SwapUTxO{swapAddress=(UserAddress addr),..} =
    object [ "swap_address" .= addr
           , "tx_hash" .= swapTxHash
           , "output_index" .= swapOutputIndex
           , "amount" .= swapValue
           , "swap_info" .= swapDatum
           ]

