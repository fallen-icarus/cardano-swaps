{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.OneWaySwap.Utils where

import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import Plutus.Script.Utils.Value

-- | 26 ADA is the default. This is what the cardano-swaps spending validator requires.
minUTxOSpendRef :: Integer
minUTxOSpendRef = 26_000_000

-- | 18 ADA is the default. This is what the cardano-loans minting policy requires.
minUTxOMintRef :: Integer
minUTxOMintRef = 18_000_000

data UnsafeDatum = UnsafeDatum
  { unsafeBeaconId :: CurrencySymbol
  , unsafeBeaconName :: TokenName
  , unsafeOfferId :: CurrencySymbol
  , unsafeOfferName :: TokenName
  , unsafeAskId :: CurrencySymbol
  , unsafeAskName :: TokenName
  , unsafeSwapPrice :: (Integer,Integer)
  }

instance PlutusTx.ToData UnsafeDatum where
  toBuiltinData UnsafeDatum{..} = PlutusTx.dataToBuiltinData $
    PlutusTx.Constr 0 
      [ PlutusTx.toData unsafeBeaconId
      , PlutusTx.toData unsafeBeaconName
      , PlutusTx.toData unsafeOfferId
      , PlutusTx.toData unsafeOfferName
      , PlutusTx.toData unsafeAskId
      , PlutusTx.toData unsafeAskName
      , PlutusTx.toData unsafeSwapPrice
      ]
