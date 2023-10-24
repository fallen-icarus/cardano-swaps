{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.OneWaySwap.Utils where

import qualified PlutusTx
import Plutus.V2.Ledger.Api (TxOutRef)
import PlutusTx.Prelude hiding (Semigroup (..))
import Plutus.Script.Utils.Value

-- | 26 ADA is the default. This is what the cardano-swaps spending validator requires.
minUTxOSpendRef :: Integer
minUTxOSpendRef = 26_000_000

-- | 19 ADA is the default. This is what the cardano-loans minting policy requires.
minUTxOMintRef :: Integer
minUTxOMintRef = 16_000_000

data UnsafeDatum = UnsafeDatum
  { unsafeBeaconId :: CurrencySymbol
  , unsafePairBeacon :: TokenName
  , unsafeOfferId :: CurrencySymbol
  , unsafeOfferName :: TokenName
  , unsafeOfferBeacon :: TokenName
  , unsafeAskId :: CurrencySymbol
  , unsafeAskName :: TokenName
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
      , PlutusTx.toData unsafeSwapPrice
      , PlutusTx.toData unsafePrevInput
      ]
