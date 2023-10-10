{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.TwoWaySwap.Utils where

import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import Plutus.Script.Utils.Value
import Plutus.V2.Ledger.Api (TxOutRef)

-- | 27 ADA is the default. This is what the cardano-swaps spending validator requires.
minUTxOSpendRef :: Integer
minUTxOSpendRef = 27_000_000

-- | 22 ADA is the default. This is what the cardano-loans minting policy requires.
minUTxOMintRef :: Integer
minUTxOMintRef = 22_000_000

data UnsafeDatum = UnsafeDatum
  { unsafeBeaconId :: CurrencySymbol
  , unsafeBeaconName :: TokenName
  , unsafeAsset1Id :: CurrencySymbol
  , unsafeAsset1Name :: TokenName
  , unsafeAsset2Id :: CurrencySymbol
  , unsafeAsset2Name :: TokenName
  , unsafeForwardPrice :: (Integer,Integer)
  , unsafeReversePrice :: (Integer,Integer)
  , unsafePrevInput :: Maybe TxOutRef
  }

instance PlutusTx.ToData UnsafeDatum where
  toBuiltinData UnsafeDatum{..} = PlutusTx.dataToBuiltinData $
    PlutusTx.Constr 0 
      [ PlutusTx.toData unsafeBeaconId
      , PlutusTx.toData unsafeBeaconName
      , PlutusTx.toData unsafeAsset1Id
      , PlutusTx.toData unsafeAsset1Name
      , PlutusTx.toData unsafeAsset2Id
      , PlutusTx.toData unsafeAsset2Name
      , PlutusTx.toData unsafeForwardPrice
      , PlutusTx.toData unsafeReversePrice
      , PlutusTx.toData unsafePrevInput
      ]
