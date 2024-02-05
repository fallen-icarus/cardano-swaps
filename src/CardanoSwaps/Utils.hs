{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module CardanoSwaps.Utils
  ( 
    -- * On-Chain Data Types
    PlutusRational
  , AssetConfig
  , OfferAsset(..)
  , AskAsset(..)
  , TwoWayPair

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
  , readAssetConfig
  , readTokenName
  , readCurrencySymbol
  , readTxId
  , readTxOutRef
  , readPlutusRational

    -- * Misc
  , unsafeFromRight
  , showTokenName
  , unsafeToBuiltinByteString
  , scriptHash
  , datumHash
  , toLedgerScript
  , toVersionedLedgerScript
  , wrapVersionedLedgerScript
  , toCardanoApiScript

  -- * Re-exports
  , applyArguments
  , PV2.CurrencySymbol(..)
  , PV2.TokenName(..)
  , unsafeRatio
  , PV2.adaSymbol
  , PV2.adaToken
  , numerator
  , denominator
  , PV2.TxOutRef(..)
  , PV2.TxId(..)
  , PV2.SerialisedScript
  ) where

import qualified Data.Aeson as Aeson
import Lens.Micro (over)
import qualified Codec.Serialise as Serial
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Text (Text,unpack,pack,replace)
import qualified Data.ByteString.Lazy as LBS
import Data.String (fromString)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Base16 as Base16
import Relude (toShort,encodeUtf8)

import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusCore.MkPlc as PLC
import qualified UntypedPlutusCore as UPLC
import qualified Cardano.Api as Api 
import Cardano.Api.Shelley (fromPlutusData,PlutusScript(..))
import PlutusLedgerApi.V1.Bytes (fromHex,bytes,encodeByteString,LedgerBytesError)
import Ledger.Tx.CardanoAPI.Internal (fromCardanoScriptData)
import qualified Ledger as L
import PlutusTx.Ratio (fromGHC,unsafeRatio,numerator,denominator)
import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
type PlutusRational = PlutusTx.Rational
type AssetConfig = (PV2.CurrencySymbol,PV2.TokenName)

-------------------------------------------------
-- Off-Chain Data Types
-------------------------------------------------
newtype OfferAsset = OfferAsset { unOfferAsset :: AssetConfig }
newtype AskAsset = AskAsset { unAskAsset :: AssetConfig }
type TwoWayPair = (AssetConfig,AssetConfig)

-------------------------------------------------
-- Serialization
-------------------------------------------------
toJSONValue :: PV2.ToData a => a -> Aeson.Value
toJSONValue = Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema
            . Api.unsafeHashableScriptData
            . fromPlutusData
            . PV2.toData

writeScript :: FilePath -> PV2.SerialisedScript -> IO (Either (Api.FileError ()) ())
writeScript file script = 
  Api.writeFileTextEnvelope @(Api.PlutusScript Api.PlutusScriptV2) (Api.File file) Nothing $ 
    PlutusScriptSerialised script

writeData :: PV2.ToData a => FilePath -> a -> IO ()
writeData file = LBS.writeFile file . Aeson.encode . toJSONValue

decodeDatum :: (PV2.FromData a) => Aeson.Value -> Maybe a
decodeDatum = either (const Nothing) (PV2.fromBuiltinData . fromCardanoScriptData)
            . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

parseScriptFromCBOR :: String -> PV2.SerialisedScript
parseScriptFromCBOR script =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode validator: " <> show e
    Right bytes' -> toShort bytes'
 where
  base16Bytes = encodeUtf8 script

dataFromCBOR :: String -> Either LedgerBytesError PV2.Data
dataFromCBOR = fmap Serial.deserialise . decodeHex

decodeHex :: String -> Either LedgerBytesError LBS.ByteString
decodeHex = fmap (fromStrict . bytes) . fromHex . fromString

toCBOR :: Serial.Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . Serial.serialise

-------------------------------------------------
-- Functions for parsing user input.
-------------------------------------------------
-- | Parse `AssetConfig` from user supplied `String`. The input is expected to either be
-- "lovelace" or of the form "policy_id.asset_name".
readAssetConfig :: String -> Either String AssetConfig
readAssetConfig s =
    if s == "lovelace" then Right (PV2.adaSymbol,PV2.adaToken)
    else (,) <$> readCurrencySymbol policy <*> readTokenName (drop 1 name)
  where
    (policy,name) = span (/='.') s

-- | Parse `CurrencySymbol` from user supplied `String`.
readCurrencySymbol :: String -> Either String PV2.CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.CurrencySymbol bytes'
  Left msg                   -> Left $ show msg

-- | Parse `TokenName` from user supplied `String`.
readTokenName :: String -> Either String PV2.TokenName
readTokenName s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.TokenName bytes'
  Left msg                   -> Left $ show msg

-- | Parse `TxId` from user supplied `String`.
readTxId :: String -> Either String PV2.TxId
readTxId s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.TxId bytes'
  Left msg                   -> Left $ show msg

readTxOutRef :: String -> Either String PV2.TxOutRef
readTxOutRef s = PV2.TxOutRef <$> readTxId txHash <*> readIndex (drop 1 index)
  where
    (txHash,index) = span (/='#') s

    readIndex :: String -> Either String Integer
    readIndex i = case readMaybe i of
      Nothing -> Left $ "could not convert: " <> i
      Just i' -> Right i'

-- | Parse `PlutusRational` from user supplied `String` of either a decimal or a fraction.
readPlutusRational :: String -> Either String PlutusRational
readPlutusRational s = case fromGHC <$> (readMaybeRatio sample <|> readMaybeDouble sample) of
    Nothing -> Left $ "could not convert: " <> s
    Just r -> Right r
  where
    -- Replace / with % since Haskell fractions use % while humans use /.
    sample :: String
    sample = unpack $ replace "/" "%" $ pack s 

    readMaybeRatio :: String -> Maybe Rational
    readMaybeRatio = readMaybe

    readMaybeDouble :: String -> Maybe Rational
    readMaybeDouble = fmap toRational . readMaybe @Double

-------------------------------------------------
-- Misc
-------------------------------------------------
toCardanoApiScript :: PV2.SerialisedScript -> Api.Script Api.PlutusScriptV2
toCardanoApiScript = Api.PlutusScript Api.PlutusScriptV2 . PlutusScriptSerialised

toLedgerScript :: PV2.SerialisedScript -> PV2.Script
toLedgerScript = PV2.Script

toVersionedLedgerScript :: PV2.SerialisedScript -> PV2.Versioned PV2.Script
toVersionedLedgerScript script = PV2.Versioned (toLedgerScript script) PV2.PlutusV2

wrapVersionedLedgerScript :: (PV2.Script -> a) -> PV2.Versioned PV2.Script -> PV2.Versioned a
wrapVersionedLedgerScript wrapper v@PV2.Versioned{PV2.unversioned} = 
  v{PV2.unversioned = wrapper unversioned}

scriptHash :: PV2.SerialisedScript -> PV2.ScriptHash
scriptHash =
  PV2.ScriptHash
    . Builtins.toBuiltin
    . Api.serialiseToRawBytes
    . Api.hashScript
    . toCardanoApiScript

datumHash :: (PV2.ToData a) => a -> PV2.DatumHash
datumHash = L.datumHash . L.Datum . PV2.dataToBuiltinData . PV2.toData

applyArguments :: PV2.SerialisedScript -> [PV2.Data] -> PV2.SerialisedScript
applyArguments p args =
    let termArgs = fmap (PLC.mkConstant ()) args
        applied t = PLC.mkIterAppNoAnn t termArgs
    in PV2.serialiseUPLC $ over UPLC.progTerm applied $ PV2.uncheckedDeserialiseUPLC p

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

-- | Show the token name in hexidecimal.
showTokenName :: PV2.TokenName -> String
showTokenName (PV2.TokenName name) = show $ PV2.PubKeyHash name

unsafeToBuiltinByteString :: String -> Builtins.BuiltinByteString
unsafeToBuiltinByteString = (\(PV2.LedgerBytes bytes') -> bytes')
                          . unsafeFromRight
                          . fromHex
                          . fromString
