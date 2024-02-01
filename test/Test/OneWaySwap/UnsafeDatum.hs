{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Test.OneWaySwap.UnsafeDatum where

import qualified PlutusTx

import CardanoSwaps.Utils

data UnsafeDatum = UnsafeDatum
  { unsafeBeaconId :: CurrencySymbol
  , unsafePairBeacon :: TokenName
  , unsafeOfferId :: CurrencySymbol
  , unsafeOfferName :: TokenName
  , unsafeOfferBeacon :: TokenName
  , unsafeAskId :: CurrencySymbol
  , unsafeAskName :: TokenName
  , unsafeAskBeacon :: TokenName
  , unsafeSwapPrice :: (Integer,Integer)
  , unsafePrevInput :: Maybe TxOutRef
  }

instance PlutusTx.ToData UnsafeDatum where
  toBuiltinData UnsafeDatum{..} = PlutusTx.dataToBuiltinData $
    PlutusTx.Constr 0 
      [ PlutusTx.toData unsafeBeaconId
      , PlutusTx.toData unsafePairBeacon
      , PlutusTx.toData unsafeOfferId
      , PlutusTx.toData unsafeOfferName
      , PlutusTx.toData unsafeOfferBeacon
      , PlutusTx.toData unsafeAskId
      , PlutusTx.toData unsafeAskName
      , PlutusTx.toData unsafeAskBeacon
      , PlutusTx.toData unsafeSwapPrice
      , PlutusTx.toData unsafePrevInput
      ]
