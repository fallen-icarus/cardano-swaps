{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CLI.Types where

import Data.Aeson

import CardanoSwaps

data Command
  = ExportScript Script FilePath
  | CreateDatum SwapDatumInfo FilePath
  | CreateSwapRedeemer SwapRedeemer FilePath
  | CreateBeaconRedeemer BeaconRedeemer FilePath
  | BeaconInfo BeaconInfo Output
  | QueryBeacons Query

data Script = BeaconPolicy AssetConfig | SwapScript

-- | This has all the info necessary to create the actual SwapDatum.
data SwapDatumInfo = 
  SwapDatumInfo 
    AssetConfig -- ^ Offer asset
    AssetConfig -- ^ Ask asset
    PlutusRational -- ^ Swap price

data Network
  = PreProdTestnet

data ApiEndpoint
  = Blockfrost String -- ^ Api key
  | Koios

-- | For when saving to file is optional
data Output = Stdout | File FilePath

data BeaconInfo = PolicyId AssetConfig | AssetName AssetConfig

data Query
  = QueryAllSwapsByTradingPair Network ApiEndpoint AssetConfig AssetConfig Output
  | QueryAllSwapsByOffer AssetConfig Output
  | QueryOwnSwaps Network ApiEndpoint SwapAddress Output
  | QueryOwnSwapsByOffer SwapAddress AssetConfig Output
  | QueryOwnSwapsByTradingPair Network ApiEndpoint SwapAddress AssetConfig AssetConfig Output

newtype SwapAddress = SwapAddress String

-- | Type that captures all info a user needs to interact with available swaps.
data SwapUTxO = SwapUTxO
  { address :: String
  , txIx :: String
  , value :: [Asset]
  , datum :: Maybe SwapDatum
  } deriving (Show)

instance ToJSON SwapUTxO where
  toJSON SwapUTxO{..} =
    object [ "swap_address" .= address
           , "utxo_id" .= txIx
           , "assets" .= value
           , "datum" .= datum
           ]

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

data Asset = Asset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: String
  } deriving (Show)

instance ToJSON Asset where
  toJSON Asset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" || assetPolicyId == ""
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]