{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CLI.Types where

import Data.Aeson
import Data.Text (Text)
import Prettyprinter
import Data.ByteString.Lazy (ByteString)
import Control.Monad (mzero)
import Servant.API (Accept(..),MimeRender(..))

import CardanoSwaps

newtype OfferAsset = OfferAsset AssetConfig
newtype AskAsset = AskAsset AssetConfig

data Command
  = ExportScript Script FilePath
  | CreateDatum InternalSwapDatum FilePath
  | CreateSwapRedeemer SwapRedeemer FilePath
  | CreateBeaconRedeemer BeaconRedeemer FilePath
  | BeaconInfo BeaconInfo Output
  | Query Query
  | Submit Network QueryEndpoint FilePath
  | ExportParams Network Output

data Script = BeaconPolicy OfferAsset | SwapScript

-- | This has all the info necessary to create the actual SwapDatum.
data InternalSwapDatum = 
  InternalSwapDatum 
    OfferAsset
    AskAsset
    PlutusRational -- ^ Swap price

data Network
  = PreProdTestnet
  | Mainnet

data QueryEndpoint
  = Koios

-- | For when saving to file is optional
data Output = Stdout | File FilePath

data Format = JSON | Pretty | Plain

data BeaconInfo 
  = PolicyId OfferAsset 
  | AssetName AskAsset
  | FullName OfferAsset AskAsset

newtype UserAddress = UserAddress Text 
  deriving (Show,Eq)

instance Pretty UserAddress where
  pretty (UserAddress addr) = pretty addr

data Query
  = QueryOwn QueryOwn
  | QueryAll QueryAll
  | QueryPersonal Network QueryEndpoint UserAddress Format Output

data QueryOwn
  = QueryOwnSwaps Network QueryEndpoint UserAddress Format Output
  | QueryOwnSwapsByOffer Network QueryEndpoint UserAddress OfferAsset Format Output
  | QueryOwnSwapsByTradingPair Network QueryEndpoint UserAddress OfferAsset AskAsset Format Output

data QueryAll
  = QueryAllSwapsByOffer Network QueryEndpoint OfferAsset Format Output
  | QueryAllSwapsByTradingPair Network QueryEndpoint OfferAsset AskAsset Format Output

newtype TxCBOR = TxCBOR ByteString

instance FromJSON TxCBOR where
  parseJSON (Object o) = do
    cborHex <- o .: "cborHex"
    case decodeHex cborHex of
      Right cbor -> return $ TxCBOR cbor
      Left _ -> mzero
  parseJSON _ = mzero

data CBOR

instance Accept CBOR where
  contentType = pure "application/cbor"

instance MimeRender CBOR TxCBOR where
  mimeRender _ (TxCBOR cbor) = cbor

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

-- | Type that captures all info a user needs to interact with available swaps.
data SwapUTxO = SwapUTxO
  { swapAddress :: UserAddress
  , swapTxHash :: Text
  , swapOutputIndex :: Integer
  , swapValue :: [Asset]
  , swapDatum :: Maybe SwapDatum
  } deriving (Show,Eq)

instance Ord SwapUTxO where
  (SwapUTxO _ _ _ _ (Just SwapDatum{swapPrice=price1})) <= 
    (SwapUTxO _ _ _ _ (Just SwapDatum{swapPrice=price2})) = price1 <= price2
  (SwapUTxO _ _ _ _ (Just _)) <= (SwapUTxO _ _ _ _ Nothing) = True
  (SwapUTxO _ _ _ _ Nothing) <= (SwapUTxO _ _ _ _ (Just _)) = False
  _ <= _ = True

instance ToJSON SwapUTxO where
  toJSON SwapUTxO{swapAddress=(UserAddress addr),..} =
    object [ "swap_address" .= addr
           , "tx_hash" .= swapTxHash
           , "output_index" .= swapOutputIndex
           , "amount" .= swapValue
           , "swap_datum" .= swapDatum
           ]

-------------------------------------------------
-- Orphan Instances
-------------------------------------------------
instance ToJSON SwapDatum where
  toJSON SwapDatum{..} = 
    object [ "beacon_id" .= show beaconId
           , "beacon_name" .= showTokenName beaconName
           , "offer_id" .= show offerId
           , "offer_name" .= showTokenName offerName
           , "ask_id" .= show askId
           , "ask_name" .= showTokenName askName
           , "price" .= swapPrice 
           ]

instance Pretty PlutusRational where
  pretty num = pretty (numerator num) <> " / " <> pretty (denominator num)
