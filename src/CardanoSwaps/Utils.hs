{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module CardanoSwaps.Utils
  ( 
    -- * On-Chain Data Types
    PlutusRational
  , AssetConfig

    -- * Generate Beacon Names
  , genSortedPairBeaconName
  , genUnsortedPairBeaconName
  , genOfferBeaconName
  
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

    -- * Misc
  , unsafeFromRight
  , UtxoPriceInfo(..)
  , calcWeightedPrice
  , showTokenName

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
  ) where

import Data.Aeson as Aeson
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import qualified PlutusTx.Prelude as Plutus
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
import Ledger.Tx.CardanoAPI.Internal
import PlutusTx.Ratio (unsafeRatio,numerator,denominator)
import Data.List (sort,foldl')

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
type PlutusRational = Plutus.Rational
type AssetConfig = (CurrencySymbol,TokenName)

-------------------------------------------------
-- Generate Beacon Name
-------------------------------------------------
-- | Generate the beacon asset name by hashing asset1 ++ asset2. The trading pair is first
-- sorted so that the beacon name is independent of the ordering. This is used for two-way swaps.
genSortedPairBeaconName :: AssetConfig -> AssetConfig -> TokenName
genSortedPairBeaconName assetX assetY =
  let [((CurrencySymbol sym1'),(TokenName name1)),((CurrencySymbol sym2'),(TokenName name2))] =
        sort [assetX,assetY]
      sym1 = 
        if sym1' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym1'
      sym2 = 
        if sym2' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym2'
  in TokenName $ Plutus.sha2_256 $ sym1 <> name1 <> sym2 <> name2

-- | Generate the beacon asset name by hashing offer ++ ask. This is used for one-way swaps.
genUnsortedPairBeaconName :: AssetConfig -> AssetConfig -> TokenName
genUnsortedPairBeaconName assetX assetY =
  let [((CurrencySymbol sym1'),(TokenName name1)),((CurrencySymbol sym2'),(TokenName name2))] =
        [assetX,assetY]
      sym1 = 
        if sym1' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym1'
      sym2 = 
        if sym2' == "" 
        then unsafeToBuiltinByteString "00" 
        else sym2'
  in TokenName $ Plutus.sha2_256 $ sym1 <> name1 <> sym2 <> name2

-- | Generate the beacon asset name by hashing the ask asset policy id and name.
genOfferBeaconName :: CurrencySymbol -> TokenName -> TokenName
genOfferBeaconName (CurrencySymbol sym) (TokenName name) =
  TokenName $ Plutus.sha2_256 $ sym <> name

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

-------------------------------------------------
-- Misc
-------------------------------------------------
unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

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

unsafeToBuiltinByteString :: String -> BuiltinByteString
unsafeToBuiltinByteString = (\(LedgerBytes bytes') -> bytes')
                          . unsafeFromRight
                          . fromHex
                          . fromString

