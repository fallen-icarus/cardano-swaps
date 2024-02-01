{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Test.TwoWaySwap.UnsafeDatum where

import qualified PlutusTx

import CardanoSwaps.Utils

data UnsafeDatum = UnsafeDatum
  { unsafeBeaconId :: CurrencySymbol
  , unsafePairBeacon :: TokenName
  , unsafeAsset1Id :: CurrencySymbol
  , unsafeAsset1Name :: TokenName
  , unsafeAsset1Beacon :: TokenName
  , unsafeAsset2Id :: CurrencySymbol
  , unsafeAsset2Name :: TokenName
  , unsafeAsset2Beacon :: TokenName
  , unsafeAsset1Price :: (Integer,Integer)
  , unsafeAsset2Price :: (Integer,Integer)
  , unsafePrevInput :: Maybe TxOutRef
  }

instance PlutusTx.ToData UnsafeDatum where
  toBuiltinData UnsafeDatum{..} = PlutusTx.dataToBuiltinData $
    PlutusTx.Constr 0 
      [ PlutusTx.toData unsafeBeaconId
      , PlutusTx.toData unsafePairBeacon
      , PlutusTx.toData unsafeAsset1Id
      , PlutusTx.toData unsafeAsset1Name
      , PlutusTx.toData unsafeAsset1Beacon
      , PlutusTx.toData unsafeAsset2Id
      , PlutusTx.toData unsafeAsset2Name
      , PlutusTx.toData unsafeAsset2Beacon
      , PlutusTx.toData unsafeAsset1Price
      , PlutusTx.toData unsafeAsset2Price
      , PlutusTx.toData unsafePrevInput
      ]
