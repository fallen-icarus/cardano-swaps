{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.TwoWaySwap.Utils where

import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import Plutus.Script.Utils.Value
import Plutus.V2.Ledger.Api (TxOutRef)

-- | 31 ADA is the default. This is what the cardano-swaps spending validator requires.
minUTxOSpendRef :: Integer
minUTxOSpendRef = 31_000_000

-- | 22 ADA is the default. This is what the cardano-loans minting policy requires.
minUTxOMintRef :: Integer
minUTxOMintRef = 18_000_000

data UnsafeDatum = UnsafeDatum
  { unsafeBeaconId :: CurrencySymbol
  , unsafePairBeacon :: TokenName
  , unsafeAsset1Id :: CurrencySymbol
  , unsafeAsset1Name :: TokenName
  , unsafeAsset1Beacon :: TokenName
  , unsafeAsset2Id :: CurrencySymbol
  , unsafeAsset2Name :: TokenName
  , unsafeAsset2Beacon :: TokenName
  , unsafeForwardPrice :: (Integer,Integer)
  , unsafeReversePrice :: (Integer,Integer)
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
      , PlutusTx.toData unsafeForwardPrice
      , PlutusTx.toData unsafeReversePrice
      , PlutusTx.toData unsafePrevInput
      ]
