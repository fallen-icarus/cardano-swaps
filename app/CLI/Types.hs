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

data Script = BeaconPolicy SwapConfig | SwapScript SwapConfig

data Datum = SwapDatum SwapDatum | WeightedPrice [UtxoPriceInfo]