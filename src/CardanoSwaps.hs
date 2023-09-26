{-# LANGUAGE DataKinds             #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CardanoSwaps
(
  -- * On-Chain Data Types
  PlutusRational,
  AssetConfig,
  SwapDatum(..),
  SwapRedeemer(..),
  BeaconRedeemer(..),

  -- * Contracts
  swapScript,
  swapValidator,
  swapValidatorHash,

  beaconScript,
  beaconMintingPolicy,
  beaconMintingPolicyHash,
  beaconCurrencySymbol,

  -- * Serialization
  writeData,
  writeScript,
  decodeDatum,

  -- * Parsers
  readTokenName,
  readCurrencySymbol,

  -- * Off-Chain Helper Functions
  genBeaconName,
  UtxoPriceInfo(..),
  calcWeightedPrice,
  showTokenName,

  -- * Misc Functions
  unsafeFromRight,
  dataFromCBOR,
  toCBOR,

  -- * Re-exports
  Ledger.scriptSize,
  CurrencySymbol(..),
  TokenName(..),
  unsafeRatio,
  adaSymbol,
  adaToken
) where

import Prelude hiding (fromInteger)
import Data.Aeson as Aeson
import Control.Monad
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import qualified PlutusTx.Prelude as Plutus
import GHC.Generics (Generic)
import Codec.Serialise hiding (decode,encode)
import Ledger (Script(..),applyArguments,scriptSize)
import Cardano.Api hiding (Script,Address)
import Cardano.Api.Shelley (PlutusScript (..))
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.String (fromString)
import Ledger.Bytes (fromHex,bytes,encodeByteString)
import qualified Data.Map as Map
import Ledger.Tx.CardanoAPI.Internal
import Plutus.Script.Utils.V2.Scripts
import PlutusTx.Ratio (unsafeRatio)
import Data.FileEmbed
import Data.List (foldl')

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
type PlutusRational = Plutus.Rational
type AssetConfig = (CurrencySymbol,TokenName)

data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol
  , beaconName :: TokenName
  , offerId :: CurrencySymbol
  , offerName :: TokenName
  , askId :: CurrencySymbol
  , askName :: TokenName
  , swapPrice :: PlutusRational
  }
  deriving (Generic,Show)

data SwapRedeemer
  = CloseOrUpdate
  | Swap
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateSwap [AssetConfig] -- ^ The assets being asked for.
  | BurnBeacons
  deriving (Generic,Show)

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- Blueprints
-------------------------------------------------
newtype Blueprints = Blueprints (Map.Map String String)
  deriving (Show)

instance FromJSON Blueprints where
  parseJSON (Object o) = 
    Blueprints . Map.fromList <$> 
      (o .: "validators" >>= mapM (\(Object o') -> (,) <$> o' .: "title" <*> o' .: "compiledCode"))
  parseJSON _ = mzero

blueprints :: Map.Map String String
blueprints = 
  case decode $ LBS.fromStrict $(embedFile "aiken/plutus.json") of
    Nothing -> error "Failed to decode cardano-swaps' blueprint file"
    Just (Blueprints bs) -> bs

parseScriptFromCBOR :: String -> Ledger.Script
parseScriptFromCBOR cbor = 
  case fmap (deserialise . fromStrict . bytes) . fromHex $ fromString cbor of
    Left err -> error err
    Right script -> script

swapScript :: Ledger.Script
swapScript = parseScriptFromCBOR $ blueprints Map.! "cardano_swaps.spend"

swapValidator :: Validator
swapValidator = Validator swapScript

swapValidatorHash :: ValidatorHash
swapValidatorHash = validatorHash swapValidator

-- This script is still missing the `AssetConfig`.
partialBeaconScript :: Ledger.Script
partialBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_swaps.mint")
    [toData swapValidatorHash]

beaconScript :: AssetConfig -> Ledger.Script
beaconScript cfg = applyArguments partialBeaconScript [toData cfg]

beaconMintingPolicy :: AssetConfig -> MintingPolicy
beaconMintingPolicy = MintingPolicy . beaconScript

beaconMintingPolicyHash :: AssetConfig -> MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash . beaconMintingPolicy

beaconCurrencySymbol :: AssetConfig -> CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol . beaconMintingPolicy

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

-------------------------------------------------
-- Other Off-Chain functions
-------------------------------------------------
-- | Generate the beacon asset name by hashing the ask asset policy id and name.
genBeaconName :: AssetConfig -> TokenName
genBeaconName ((CurrencySymbol sym),(TokenName name)) =
  TokenName $ Plutus.sha2_256 $ sym <> name

data UtxoPriceInfo = UtxoPriceInfo
  { utxoAmount :: Integer -- ^ The amount of the offer asset in this UTxO.
  , price :: PlutusRational -- ^ The price for this UTxO.
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

-- | Show the token name in hexidecimal.
showTokenName :: TokenName -> String
showTokenName (TokenName name) = show $ PubKeyHash name

-------------------------------------------------
-- Misc
-------------------------------------------------
unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

dataFromCBOR :: String -> Either String Data
dataFromCBOR = fmap (deserialise . fromStrict . bytes) . fromHex . fromString

toCBOR :: Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . serialise
