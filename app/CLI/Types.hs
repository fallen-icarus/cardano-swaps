{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CLI.Types where

import Data.Aeson

import CardanoSwaps

data Command
  = ExportScript Script FilePath
  | CreateSwapDatum Datum FilePath
  | CreateSwapRedeemer SwapRedeemer FilePath
  | CreateBeaconRedeemer BeaconRedeemer FilePath
  | QueryBeacons Query

data Script = BeaconPolicy SwapConfig | SwapScript SwapConfig

data Datum = SwapDatum SwapDatum | WeightedPrice [UtxoPriceInfo]

data Query
  = QueryAvailableSwaps Network ApiEndpoint SwapConfig Output
  | QueryOwnUTxOs Network ApiEndpoint SwapAddress Output

data Network
  = PreProdTestnet

data ApiEndpoint
  = Blockfrost String -- ^ Api key
  | Koios

-- | For when saving to file is optional
data Output = Stdout | File FilePath

newtype SwapAddress = SwapAddress String

instance Show SwapAddress where
  show (SwapAddress a) = a

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
  toJSON (SwapPrice price) = object [ "price" .= price ]
  toJSON (BeaconSymbol sym) = object [ "beacon_id" .= show sym ]

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