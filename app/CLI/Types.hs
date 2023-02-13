{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Types where

import Data.ByteString (ByteString)
import Data.Aeson

import CardanoSwaps

data Asset = Ada | Asset !CurrencySymbol !TokenName

-- | For when saving to file is optional
data Output = StdOut | File !FilePath

data Command
  = SwapCmd SwapCmd
  -- | BeaconCmd BeaconCmd

data SwapCmd
  = SwapScript !Asset !Asset !FilePath
  | SwapDatum !DatumPrice !FilePath
  | SwapRedeemer !Action !FilePath

data DatumPrice = NewDatum !Price | WeightedPrice ![UtxoPriceInfo]