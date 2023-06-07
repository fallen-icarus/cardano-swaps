{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE StrictData #-}

module CardanoSwaps 
(
  SwapConfig(..),
  SwapDatum(..),
  SwapRedeemer(..),
  BeaconRedeemer(..),
  Blueprints,
  CurrencySymbol(..),
  TokenName(..),
  DappScripts(..),
  unValidatorScript,
  unMintingPolicyScript,

  adaSymbol,
  adaToken,
  Plutus.unsafeRatio,
  readBlueprints,
  applySwapParams,
  applyBeaconParams,
  readCurrencySymbol,
  readTokenName,
  writeData,
  writeScript,
  toCBOR,
  dataFromCBOR,
  unsafeFromRight,
  decodeDatum,
  PlutusRational,
  UtxoPriceInfo(..),
  calcWeightedPrice,
  parseBlueprints,
  genScripts
) where

import Data.Aeson as Aeson
import Control.Monad
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.Prelude as Plutus
import GHC.Generics (Generic)
import Codec.Serialise hiding (decode,encode)
import Ledger (Script(..),applyArguments)
import Cardano.Api hiding (Script)
import Cardano.Api.Shelley (PlutusScript (..))
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.String (fromString)
import Ledger.Bytes (fromHex,bytes,encodeByteString)
import Data.Text (Text)
import qualified Data.Map as Map
import Ledger.Tx.CardanoAPI.Internal
import Data.List (foldl')
import Plutus.Script.Utils.V2.Scripts

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data SwapConfig = SwapConfig
  { swapOffer :: (CurrencySymbol,TokenName)
  , swapAsk :: (CurrencySymbol,TokenName)
  } deriving (Generic)

-- | Need a custom instance since Aiken treats tuples differently.
instance ToData SwapConfig where
  toBuiltinData (SwapConfig (x1,x2) (y1,y2)) = dataToBuiltinData $
    Constr 0 [List [toData x1, toData x2],List [toData y1, toData y2]]

data SwapDatum
  = BeaconSymbol CurrencySymbol
  | SwapPrice Plutus.Rational
  deriving (Generic)

data SwapRedeemer
  = Close
  | Update
  | Swap
  deriving (Generic)

data BeaconRedeemer
  = MintBeacon
  | BurnBeacon
  deriving (Generic)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- Functions and Types for working with blueprints
-------------------------------------------------
type Title = String
type CBOR = String
type Blueprints = Map.Map Title CBOR

newtype Blueprints' = Blueprints' [(Title,CBOR)]
  deriving (Show)

instance FromJSON Blueprints' where
  parseJSON (Object o) = 
    Blueprints' 
      <$> (o .: "validators" >>= 
            mapM (\(Object o') -> (,) <$> o' .: "title" <*> o' .: "compiledCode"))
  parseJSON _ = mzero

readBlueprints :: FilePath -> IO Blueprints
readBlueprints = fmap parseBlueprints . LBS.readFile

parseBlueprints :: LBS.ByteString -> Blueprints
parseBlueprints = toBlueprints . decode

toBlueprints :: Maybe Blueprints' -> Blueprints
toBlueprints (Just (Blueprints' bs)) = Map.fromList bs
toBlueprints Nothing = error "Failed to decode blueprint file"

data DappScripts = DappScripts
  { spendingValidator :: Validator
  , spendingValidatorHash :: ValidatorHash
  , beaconPolicy :: MintingPolicy
  , beaconPolicyHash :: MintingPolicyHash
  , beaconCurrencySymbol :: CurrencySymbol
  } deriving (Generic)

genScripts :: SwapConfig -> Blueprints -> DappScripts
genScripts cfg bs = DappScripts
    { spendingValidator = spendVal
    , spendingValidatorHash = spendValHash
    , beaconPolicy = beacon
    , beaconPolicyHash = beaconHash
    , beaconCurrencySymbol = scriptCurrencySymbol beacon
    }
  where spendVal = Validator $ applySwapParams cfg $ bs Map.! "cardano_swaps.spend"
        spendValHash = validatorHash spendVal
        beacon = MintingPolicy $ applyBeaconParams spendValHash $ bs Map.! "cardano_swaps.mint"
        beaconHash = mintingPolicyHash beacon

fromHex' :: String -> ByteString
fromHex' s = case fmap bytes $ fromHex $ fromString s of
  Right b -> b
  Left err -> error err

parseScriptFromCBOR :: String -> Ledger.Script
parseScriptFromCBOR = deserialise . fromStrict . fromHex'

dataFromCBOR :: String -> Data
dataFromCBOR = deserialise . fromStrict . fromHex'

toCBOR :: Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . serialise

applySwapParams :: SwapConfig -> String -> Ledger.Script
applySwapParams cfg cbor = applyArguments paramScript [toData cfg]
  where paramScript = parseScriptFromCBOR cbor

applyBeaconParams :: ValidatorHash -> String -> Ledger.Script
applyBeaconParams valHash cbor = applyArguments paramScript [toData valHash]
  where paramScript = parseScriptFromCBOR cbor

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

-------------------------------------------------
-- | Functions for parsing user input.
-------------------------------------------------
-- | Parse Currency from user supplied String
readCurrencySymbol :: String -> Either String CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ CurrencySymbol bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse TokenName from user supplied String
readTokenName :: String -> Either String TokenName
readTokenName s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TokenName bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

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

-------------------------------------------------
-- | Other Off-Chain functions
-------------------------------------------------
type PlutusRational = Plutus.Rational
data UtxoPriceInfo = UtxoPriceInfo
  { utxoAmount :: Integer
  , price :: PlutusRational
  } deriving (Show)

-- | Helper function to calculate the weighted price.
-- Will match the weighted price calculation done by script.
calcWeightedPrice :: [UtxoPriceInfo] -> PlutusRational
calcWeightedPrice xs = snd $ foldl' foo (0,Plutus.fromInteger 0) xs
  where 
    foo :: (Integer,PlutusRational) -> UtxoPriceInfo -> (Integer,PlutusRational)
    foo (runningTot,wp) UtxoPriceInfo{..} =
      let newAmount = runningTot + utxoAmount
          newWp = Plutus.unsafeRatio runningTot newAmount Plutus.* wp Plutus.+
                  Plutus.unsafeRatio utxoAmount newAmount Plutus.* price
      in (newAmount,newWp)