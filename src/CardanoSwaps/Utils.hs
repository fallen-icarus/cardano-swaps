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
  , getScriptSize

    -- * Time
  , PV3.POSIXTime(..)
  , L.Slot(..)
  , slotToPOSIXTime
  , posixTimeToSlot
  , preprodTimeConfig
  , mainnetTimeConfig
  , toNearestMinute

  -- * Re-exports
  , applyArguments
  , PV3.CurrencySymbol(..)
  , PV3.TokenName(..)
  , unsafeRatio
  , PV3.adaSymbol
  , PV3.adaToken
  , numerator
  , denominator
  , PV3.TxOutRef(..)
  , PV3.TxId(..)
  , PV3.SerialisedScript
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
import Cardano.Api.Plutus (fromPlutusData,PlutusScript(..))
import PlutusLedgerApi.V1.Bytes (fromHex,bytes,encodeByteString,LedgerBytesError)
import Ledger.Tx.CardanoAPI.Internal (fromCardanoScriptData)
import qualified Ledger as L
import PlutusTx.Ratio (fromGHC,unsafeRatio,numerator,denominator)
import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.Script.Utils.Scripts as PV3
import qualified Plutus.Script.Utils.Data as PSU
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V3 as PV3

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
type PlutusRational = PlutusTx.Rational
type AssetConfig = (PV3.CurrencySymbol,PV3.TokenName)

-------------------------------------------------
-- Off-Chain Data Types
-------------------------------------------------
newtype OfferAsset = OfferAsset { unOfferAsset :: AssetConfig }
newtype AskAsset = AskAsset { unAskAsset :: AssetConfig }
type TwoWayPair = (AssetConfig,AssetConfig)

-------------------------------------------------
-- Serialization
-------------------------------------------------
toJSONValue :: PV3.ToData a => a -> Aeson.Value
toJSONValue = Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema
            . Api.unsafeHashableScriptData
            . fromPlutusData
            . PV3.toData

writeScript :: FilePath -> PV3.SerialisedScript -> IO (Either (Api.FileError ()) ())
writeScript file script =
  Api.writeFileTextEnvelope @(Api.PlutusScript Api.PlutusScriptV3) (Api.File file) Nothing $
    PlutusScriptSerialised script

writeData :: PV3.ToData a => FilePath -> a -> IO ()
writeData file = LBS.writeFile file . Aeson.encode . toJSONValue

decodeDatum :: (PV3.FromData a) => Aeson.Value -> Maybe a
decodeDatum = either (const Nothing) (PV3.fromBuiltinData . fromCardanoScriptData)
            . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

parseScriptFromCBOR :: String -> PV3.SerialisedScript
parseScriptFromCBOR script =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode validator: " <> show e
    Right bytes' -> toShort bytes'
 where
  base16Bytes = encodeUtf8 script

dataFromCBOR :: String -> Either LedgerBytesError PV3.Data
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
    if s == "lovelace" then Right (PV3.adaSymbol,PV3.adaToken)
    else (,) <$> readCurrencySymbol policy <*> readTokenName (drop 1 name)
  where
    (policy,name) = span (/='.') s

-- | Parse `CurrencySymbol` from user supplied `String`.
readCurrencySymbol :: String -> Either String PV3.CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (PV3.LedgerBytes bytes') -> Right $ PV3.CurrencySymbol bytes'
  Left msg                   -> Left $ show msg

-- | Parse `TokenName` from user supplied `String`.
readTokenName :: String -> Either String PV3.TokenName
readTokenName s = case fromHex $ fromString s of
  Right (PV3.LedgerBytes bytes') -> Right $ PV3.TokenName bytes'
  Left msg                   -> Left $ show msg

-- | Parse `TxId` from user supplied `String`.
readTxId :: String -> Either String PV3.TxId
readTxId s = case fromHex $ fromString s of
  Right (PV3.LedgerBytes bytes') -> Right $ PV3.TxId bytes'
  Left msg                   -> Left $ show msg

readTxOutRef :: String -> Either String PV3.TxOutRef
readTxOutRef s = PV3.TxOutRef <$> readTxId txHash <*> readIndex (drop 1 index)
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
-- Time
-------------------------------------------------
-- | Datatype to configure the length (ms) of one slot and the beginning of the
-- first slot.
data SlotConfig = SlotConfig
  { scSlotLength :: !Integer
  -- ^ Length (number of milliseconds) of one slot
  , scSlotZeroTime :: !PV3.POSIXTime
  -- ^ Beginning of slot 0 (in milliseconds)
  } deriving (Eq, Show)

-- | Get the starting 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToBeginPOSIXTime :: SlotConfig -> L.Slot -> PV3.POSIXTime
slotToBeginPOSIXTime SlotConfig{scSlotLength, scSlotZeroTime} (L.Slot n) =
  let msAfterBegin = n * scSlotLength
   in PV3.POSIXTime $ PV3.getPOSIXTime scSlotZeroTime + msAfterBegin

-- | Convert a 'POSIXTime' to 'Slot' given a 'SlotConfig'.
posixTimeToEnclosingSlot :: SlotConfig -> PV3.POSIXTime -> L.Slot
posixTimeToEnclosingSlot SlotConfig{scSlotLength, scSlotZeroTime} (PV3.POSIXTime t) =
  let timePassed = t - PV3.getPOSIXTime scSlotZeroTime
      slotsPassed = PlutusTx.divide timePassed scSlotLength
   in L.Slot slotsPassed

slotToPOSIXTime :: SlotConfig -> L.Slot -> PV3.POSIXTime
slotToPOSIXTime = slotToBeginPOSIXTime

posixTimeToSlot :: SlotConfig -> PV3.POSIXTime -> L.Slot
posixTimeToSlot = posixTimeToEnclosingSlot

-- | The preproduction testnet has not always had 1 second slots. Therefore, the default settings
-- for SlotConfig are not usable on the testnet. To fix this, the proper SlotConfig must be
-- normalized to "pretend" that the testnet has always used 1 second slot intervals.
--
-- The normalization is done by taking a slot time and subtracting the slot number from it.
-- For example, slot 56919374 occurred at 1712602574 POSIXTime. So subtracting the slot number 
-- from the time yields the normalized 0 time. The final number needs to be converted to
-- milliseconds.
preprodTimeConfig :: SlotConfig
preprodTimeConfig = SlotConfig 1000 $ PV3.POSIXTime $ (1712603045 - 56919845) * 1000

-- | The mainnet config must also be normalized.
mainnetTimeConfig :: SlotConfig
mainnetTimeConfig = SlotConfig 1000 $ PV3.POSIXTime $ (1712661664 - 121095373) * 1000

toNearestMinute :: PV3.POSIXTime -> PV3.POSIXTime
toNearestMinute time =
  let remainder = time `mod` 60_000
  in if remainder >= 30_000
     then time + (60_000 - remainder)
     else time - remainder

-------------------------------------------------
-- Misc
-------------------------------------------------
toCardanoApiScript :: PV3.SerialisedScript -> Api.Script Api.PlutusScriptV3
toCardanoApiScript = Api.PlutusScript Api.PlutusScriptV3 . PlutusScriptSerialised

toLedgerScript :: PV3.SerialisedScript -> PV3.Script
toLedgerScript = PV3.Script

toVersionedLedgerScript :: PV3.SerialisedScript -> PV3.Versioned PV3.Script
toVersionedLedgerScript script = PV3.Versioned (toLedgerScript script) PV3.PlutusV3

wrapVersionedLedgerScript :: (PV3.Script -> a) -> PV3.Versioned PV3.Script -> PV3.Versioned a
wrapVersionedLedgerScript wrapper v@PV3.Versioned{PV3.unversioned} = 
  v{PV3.unversioned = wrapper unversioned}

scriptHash :: PV3.SerialisedScript -> PV3.ScriptHash
scriptHash =
  PV3.ScriptHash
    . Builtins.toBuiltin
    . Api.serialiseToRawBytes
    . Api.hashScript
    . toCardanoApiScript

datumHash :: (PV3.ToData a) => a -> PV3.DatumHash
datumHash = PSU.datumHash . L.Datum . PV3.dataToBuiltinData . PV3.toData

-- | plutus-ledger only provides JSON instances for the V1 `TxOutRef`, so the
-- V3 version delegates to them to keep the CLI's JSON format unchanged.
instance Aeson.ToJSON PV3.TxOutRef where
  toJSON (PV3.TxOutRef (PV3.TxId h) ix) = Aeson.toJSON $ PV1.TxOutRef (PV1.TxId h) ix

applyArguments :: PV3.SerialisedScript -> [PV3.Data] -> PV3.SerialisedScript
applyArguments p args =
    let termArgs = fmap (PLC.mkConstant ()) args
        applied t = PLC.mkIterAppNoAnn t termArgs
    in PV3.serialiseUPLC $ over UPLC.progTerm applied $ PV3.uncheckedDeserialiseUPLC p

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

-- | Show the token name in hexidecimal.
showTokenName :: PV3.TokenName -> String
showTokenName (PV3.TokenName name) = show $ PV3.PubKeyHash name

unsafeToBuiltinByteString :: String -> Builtins.BuiltinByteString
unsafeToBuiltinByteString = (\(PV3.LedgerBytes bytes') -> bytes')
                          . unsafeFromRight
                          . fromHex
                          . fromString

getScriptSize :: PV3.SerialisedScript -> Integer
getScriptSize = UPLC.serialisedSize

