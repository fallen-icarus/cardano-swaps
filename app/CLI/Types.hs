{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Types where

import Data.Aeson

import CardanoSwaps

data Asset = Ada | Asset !CurrencySymbol !TokenName

-- | For when saving to file is optional
data Output = Stdout | File !FilePath

data Command
  = SwapCmd SwapCmd
  | BeaconCmd BeaconCmd
  | QueryAvailableSwaps !Asset !Asset !Network !Output

data SwapCmd
  = ExportSwapScript !Asset !Asset !FilePath
  | CreateSwapDatum !DatumPrice !FilePath
  | CreateSwapRedeemer !Action !FilePath

data BeaconCmd
  = ExportBeaconPolicy !Asset !Asset !FilePath
  | CreateBeaconDatum !Asset !Asset !DatumPrice !FilePath
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath

data DatumPrice = NewDatum !Price | WeightedPrice ![UtxoPriceInfo]

data Network 
  = Mainnet String  -- ^ Api key
  | PreProdTestnet String  -- ^ Api key

-- | Type that captures all info a user needs to interact with available swaps.
data AvailableSwap = AvailableSwap
  { swapAddress :: String
  , swapRefScriptTxIx :: String
  , swapUTxOTxIx :: String
  , swapUTxOAmount :: [AvailableAsset]
  , swapUTxOPriceNum :: Integer
  , swapUTxOPriceDen :: Integer
  } deriving (Show)

instance ToJSON AvailableSwap where
  toJSON AvailableSwap{..} =
    object [ "swap_address" .= swapAddress
           , "swap_ref_script_id" .= swapRefScriptTxIx
           , "utxo_id" .= swapUTxOTxIx
           , "assets" .= swapUTxOAmount
           , "price_numerator" .= swapUTxOPriceNum
           , "price_denominator" .= swapUTxOPriceDen 
           ]

-- | The assets at the available utxo.
data AvailableAsset = AvailableAsset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: Integer
  } deriving (Show)

instance ToJSON AvailableAsset where
  toJSON AvailableAsset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" 
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]