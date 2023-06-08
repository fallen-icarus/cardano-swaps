{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),

  queryAvailableSwaps,
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)

import CLI.Types
import CardanoSwaps

-------------------------------------------------
-- Core Types
-------------------------------------------------
-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Newtype wrapper around the beacon asset being queried.
data BeaconId = BeaconId (String,String)

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId (currSym,tokName)) = T.pack $ currSym <> tokName

-- | An address that contains a beacon.
-- The response type of the beaconAddressList api.
newtype BeaconAddress = BeaconAddress { unBeaconAddress :: String } deriving (Show)

instance FromJSON BeaconAddress where
  parseJSON (Object o) = BeaconAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData BeaconAddress where
  toQueryParam = T.pack . unBeaconAddress

-- | The response type of the beaconInfoApi. This has all the information that may be needed.
data RawBeaconInfo = RawBeaconInfo
  { rawAddress :: String
  , rawTxHash :: String
  , rawOutputIndex :: Integer
  , rawAmount :: [RawAssetInfo]
  , rawBeaconDataHash :: Maybe String
  } deriving (Show)

instance FromJSON RawBeaconInfo where
  parseJSON (Object o) =
    RawBeaconInfo
      <$> o .: "address"
      <*> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
  parseJSON _ = mzero

-- | Blockfrost does not separate symbol and name with '.'
data RawAssetInfo = RawAssetInfo
  { rawUnit :: String  -- ^ CurrencySymbol <> TokenName
  , rawQuantity :: Integer
  } deriving (Show)

instance FromJSON RawAssetInfo where
  parseJSON (Object o) =
    RawAssetInfo
      <$> o .: "unit"
      <*> fmap read (o .: "quantity")
  parseJSON _ = mzero

instance FromJSON SwapDatum where
  parseJSON (Object o) = do
    r <- o .: "json_value" >>= return . decodeDatum
    case r of
      Just x -> return x
      Nothing -> mzero
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> Get '[JSON] [BeaconAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" BeaconAddress
    :> "utxos"
    :> Get '[JSON] [RawBeaconInfo]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] Value

beaconAddressListApi :<|> addressUtxosApi :<|> datumApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Blockfrost Query Functions
-------------------------------------------------
queryAvailableSwaps :: BlockfrostApiKey 
                    -> CurrencySymbol 
                    -> (CurrencySymbol,TokenName) 
                    -> ClientM [SwapUTxO]
queryAvailableSwaps apiKey policyId (oSym,oName) = do
  let beaconId = BeaconId (show policyId,"")
      target = if oSym == adaSymbol then "lovelace" else (show oSym <> show oName)
  -- | Get all the addresses that currently hold the beacon
  addrs <- beaconAddressListApi apiKey beaconId
  -- | Get all the UTxOs for those addresses. Filter out UTxOs that do not have the offered asset.
  utxos <- filterForAsset target . concat
       <$> mapM (\z -> addressUtxosApi apiKey z) addrs
  -- | Get the datums attached to the UTxOs.
  datums <- fetchDatumsLenient apiKey $ map rawBeaconDataHash utxos
  return $ convertToSwapUTxO utxos datums

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
filterForAsset :: String -> [RawBeaconInfo] -> [RawBeaconInfo]
filterForAsset asset = filter (isJust . find ((==asset) . rawUnit) . rawAmount)

-- | Skips ones that fail to decode.
fetchDatumsLenient :: BlockfrostApiKey -> [Maybe String] -> ClientM (Map String SwapDatum)
fetchDatumsLenient apiKey dhs =
  let go _ datumMap [] = return datumMap
      go key datumMap ((Just d):ds) = do
        i' <- fromJSON <$> datumApi key d
        case i' of
          Success i -> go key (Map.insert d i datumMap) ds
          Error _ -> go key datumMap ds
      go key datumMap (Nothing:ds) = go key datumMap ds
  in go apiKey Map.empty dhs

convertToAsset :: RawAssetInfo -> Asset
convertToAsset RawAssetInfo{rawUnit=u,rawQuantity=q} =
  if u == "lovelace"
  then Asset
        { assetPolicyId = u
        , assetTokenName = ""
        , assetQuantity = q
        }
  else Asset
        { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
        , assetTokenName = drop 56 u
        , assetQuantity = q
        }

convertToSwapUTxO :: [RawBeaconInfo] -> Map String SwapDatum -> [SwapUTxO]
convertToSwapUTxO [] _ = []
convertToSwapUTxO ((RawBeaconInfo addr tx ix amount dHash):rs) datumMap =
    info : convertToSwapUTxO rs datumMap
  where info = SwapUTxO
                { address = addr
                , txIx = tx <> "#" <> show ix
                , value = map convertToAsset amount
                , datum = fromJust $ join $ fmap (\z -> Map.lookup z datumMap) dHash
                }