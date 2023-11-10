{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CardanoSwaps.Utils
  ( 
    -- * On-Chain Data Types
    PlutusRational
  , AssetConfig
  , OfferAsset(..)
  , AskAsset(..)
  , TwoWayPair(..)

    -- * Serialization
  , writeData
  , writeScript
  , decodeDatum
  , dataFromCBOR
  , decodeHex
  , toCBOR
  , parseScriptFromCBOR

    -- * Parsing User Inputs
    -- This is just so that certain things do not need to be re-exported.
  , readTokenName
  , readCurrencySymbol
  , readTxId

    -- * Misc
  , unsafeFromRight
  , showTokenName
  , unsafeToBuiltinByteString

  -- * Re-exports
  , applyArguments
  , Ledger.scriptSize
  , CurrencySymbol(..)
  , TokenName(..)
  , unsafeRatio
  , adaSymbol
  , adaToken
  , numerator
  , denominator
  , TxOutRef(..)
  , TxId(..)
  ) where

import Data.Aeson as Aeson
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import qualified PlutusTx.Prelude as Plutus
import Codec.Serialise hiding (decode,encode)
import Ledger (Script(..),applyArguments,scriptSize)
import Cardano.Api hiding (TxId,Script,Address)
import Cardano.Api.Shelley (PlutusScript (..))
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.String (fromString)
import Ledger.Bytes (fromHex,bytes,encodeByteString)
import Ledger.Tx.CardanoAPI.Internal
import PlutusTx.Ratio (unsafeRatio,numerator,denominator)
import Prettyprinter

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
type PlutusRational = Plutus.Rational
type AssetConfig = (CurrencySymbol,TokenName)

instance Pretty PlutusRational where
  pretty num = pretty (numerator num) <> " / " <> pretty (denominator num)

-------------------------------------------------
-- Off-Chain Data Types
-------------------------------------------------
newtype OfferAsset = OfferAsset { unOfferAsset :: AssetConfig }
newtype AskAsset = AskAsset { unAskAsset :: AssetConfig }
newtype TwoWayPair = TwoWayPair { unTwoWayPair :: (AssetConfig,AssetConfig) }

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

toJSONValue :: PlutusTx.ToData a => a -> Aeson.Value
toJSONValue = scriptDataToJson ScriptDataJsonDetailedSchema
           . dataToScriptData
           . PlutusTx.toData

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . toJSONValue

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                         $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON

decodeDatum :: (FromData a) => Aeson.Value -> Maybe a
decodeDatum = unsafeFromRight . fmap (PlutusTx.fromBuiltinData . fromCardanoScriptData)
            . scriptDataFromJson ScriptDataJsonDetailedSchema

parseScriptFromCBOR :: String -> Ledger.Script
parseScriptFromCBOR cbor = 
  case fmap (deserialise . fromStrict . bytes) . fromHex $ fromString cbor of
    Left err -> error err
    Right script -> script

dataFromCBOR :: String -> Either String Data
dataFromCBOR = fmap deserialise . decodeHex

decodeHex :: String -> Either String LBS.ByteString
decodeHex = fmap (fromStrict . bytes) . fromHex . fromString

toCBOR :: Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . serialise

-------------------------------------------------
-- Functions for parsing user input.
-------------------------------------------------
-- | Parse `CurrencySymbol` from user supplied `String`.
readCurrencySymbol :: String -> Either String CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ CurrencySymbol bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse `TokenName` from user supplied `String`.
readTokenName :: String -> Either String TokenName
readTokenName s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TokenName bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse `TxId` from user supplied `String`.
readTxId :: String -> Either String TxId
readTxId s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TxId bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-------------------------------------------------
-- Misc
-------------------------------------------------
unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

-- | Show the token name in hexidecimal.
showTokenName :: TokenName -> String
showTokenName (TokenName name) = show $ PubKeyHash name

unsafeToBuiltinByteString :: String -> BuiltinByteString
unsafeToBuiltinByteString = (\(LedgerBytes bytes') -> bytes')
                          . unsafeFromRight
                          . fromHex
                          . fromString

