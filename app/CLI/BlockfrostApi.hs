{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),
  BeaconId(..),
  queryBlockfrost
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (nub,find,partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.Maybe (isJust,fromJust)

import CLI.Types
import CardanoSwaps (CurrencySymbol,TokenName,adaSymbol)

-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Wrapper around the beacon policy id
newtype BeaconId = BeaconId CurrencySymbol 

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId s) = T.pack $ show s

-- | An address that contains a beacon.
-- The response type of the assetAddressList api.
newtype SwapAddress = SwapAddress { unSwapAddress :: String } deriving (Show)

instance FromJSON SwapAddress where
  parseJSON (Object o) = SwapAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData SwapAddress where
  toQueryParam = T.pack . unSwapAddress

-- | The return type of the extendedAdressInfoApi.
data ExtendedAddressInfo = ExtendedAddressInfo
  { exAddress :: String
  , exAmount :: [ExtendedAssetInfo]
  } deriving (Show)

instance FromJSON ExtendedAddressInfo where
  parseJSON (Object o) = 
    ExtendedAddressInfo
      <$> o .: "address"
      <*> o .: "amount"
  parseJSON _ = mzero

-- | Only the address is needed.
instance ToHttpApiData ExtendedAddressInfo where
  toQueryParam = T.pack . exAddress

-- | Part of the return type for the extendedAddressInfoApi.
data ExtendedAssetInfo = ExtendedAssetInfo
  { exUnit :: String  -- ^ CurrencySymbol <> TokenName unless it is "lovelace"
  , exQuantity :: String  -- ^ The total amount at the address.
  } deriving (Show)

instance FromJSON ExtendedAssetInfo where
  parseJSON (Object o) =
    ExtendedAssetInfo
      <$> o .: "unit"
      <*> o .: "quantity"
  parseJSON _ = mzero

-- | The response type of the addressInfo api
data RawSwapInfo = RawSwapInfo
  { rawAddress :: String
  , rawTxHash :: String
  , rawIx :: Integer
  , rawAssets :: [RawAssetInfo]
  , rawDataHash :: Maybe String
  , rawReferenceScriptHash :: Maybe String
  } deriving (Show)

instance FromJSON RawSwapInfo where
  parseJSON (Object o) =
    RawSwapInfo
      <$> o .: "address"
      <*> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
      <*> o .: "reference_script_hash"
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

-- | The inline price datum type used in the datumInfoApi
data InlinePriceDatum = InlinePriceDatum
  { numerator :: Integer
  , denominator :: Integer
  } deriving (Show)

instance FromJSON InlinePriceDatum where
  parseJSON (Object o) =
    InlinePriceDatum
      <$> ((o .: "json_value") 
            >>= (.: "fields") 
            >>= (.: "fields") . (Vector.! 0) 
            >>= (.: "int") . (Vector.! 0)
          )
      <*> ((o .: "json_value") 
            >>= (.: "fields") 
            >>= (.: "fields") . (Vector.! 0)
            >>= (.: "int") . (Vector.! 1)
          )
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> Get '[JSON] [SwapAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" SwapAddress
    :> "extended"
    :> Get '[JSON] ExtendedAddressInfo

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" ExtendedAddressInfo
    :> "utxos"
    :> Get '[JSON] [RawSwapInfo]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] InlinePriceDatum

beaconAddressListApi :<|> extendedAddressInfoApi :<|> addressUTxOsApi :<|> datumInfoApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Query Blockfrost Function
-------------------------------------------------
-- | Steps for getting all necessary information:
-- 1) Get all addresses that currently hold the beacon.
-- 2) Get the extended address information for each address.
--      - returns how much of each asset is present.
-- 3) Filter out addresses that do not contain any of the offered asset.
-- 4) Get the information for each utxo at each of the remaining addresses.
--      - This also returns the reference script utxo.
--      - Blockfrost returns datum hashes even if the datum is inline.
--      - Blockfrost provides a separate query to convert datum hash to inline.
-- 5) Get the actual inline datum from the datum hash of the previous query.
-- 6) Parse the results into a user friendly format.
queryBlockfrost :: BlockfrostApiKey 
                -> CurrencySymbol 
                -> (CurrencySymbol,TokenName) 
                -> ClientM [AvailableSwap]
queryBlockfrost apiKey beaconSym (offeredSym,offeredTokName) = do
  -- | Get all the addresses that currently hold the beacon
  addrs <- beaconAddressListApi apiKey (BeaconId beaconSym)
  -- | Get the extended information for each address.
  exAddrInfo <- mapM (extendedAddressInfoApi apiKey) addrs
  -- | Filter all addresses for those that contain the target asset.
  -- Then get all of the information for each utxo at each address
  swapUTxOs <- mapM (addressUTxOsApi apiKey) $ filteredExAddresses exAddrInfo
  -- | Split each group of utxos into: reference script utxo, swappable utxos, and datum map
  groupedInfo <- mapM splitInfo swapUTxOs

  -- | Return the parsed info
  return $ concatMap convert groupedInfo

  where
    offeredUnit :: String
    offeredUnit
      | offeredSym == adaSymbol = "lovelace"
      | otherwise = show offeredSym <> drop 2 (show offeredTokName) -- ^ Drop prefix

    -- | Filter for only the addresses that contain the offered asset.
    filteredExAddresses :: [ExtendedAddressInfo] -> [ExtendedAddressInfo]
    filteredExAddresses = 
      let foo = isJust . find ((== offeredUnit) . exUnit) . exAmount
      in filter foo

    -- | Create a map from datumHash to inline datum value.
    datums :: [Maybe String] -> ClientM (Map String InlinePriceDatum)
    datums dhs =
      let go datumMap [] = return datumMap
          go datumMap (d:ds) = case d of
            Nothing -> go datumMap ds -- ^ Skip if missing datum.
            Just d' -> do
              i <- datumInfoApi apiKey d'
              go (Map.insert d' i datumMap) ds
      in go Map.empty dhs

    -- | Separate the reference script utxo, the rest of the utxos, and the unique datum hashes
    -- to lookup. Then lookup the datums and return a Map from datumHash to inline datum.
    splitInfo :: [RawSwapInfo] -> ClientM (RawSwapInfo,[RawSwapInfo],Map String InlinePriceDatum)
    splitInfo rs =
      let foo = isJust . (find ((== show beaconSym) . rawUnit)) . rawAssets
          (beaconUtxos,otherUtxos) = partition foo rs
          maybeDatumHashes = nub $ map rawDataHash otherUtxos
      in do
        iMap <- datums maybeDatumHashes
        return (head beaconUtxos, otherUtxos, iMap)

-- | Convert to the user facing data type.
convert :: (RawSwapInfo,[RawSwapInfo],Map String InlinePriceDatum) 
        -> [AvailableSwap]
convert (refUtxoInfo,otherUtxos,datumMap) = convert' otherUtxos
  where
    convert' :: [RawSwapInfo] -> [AvailableSwap]
    convert' [] = []
    convert' (x:xs) = toAvailableSwap x : convert' xs

    toAvailableSwap :: RawSwapInfo -> AvailableSwap
    toAvailableSwap ri = 
      let pAddr = rawAddress ri
          iDatum = fromJust $ Map.lookup (fromJust $ rawDataHash ri) datumMap
      in AvailableSwap
        { swapAddress = pAddr
        , swapRefScriptTxIx = rawTxHash refUtxoInfo <> "#" <> show (rawIx refUtxoInfo)
        , swapUTxOTxIx = rawTxHash ri <> "#" <> show (rawIx ri)
        , swapUTxOAmount = map toAvailableAsset $ rawAssets ri
        , swapUTxOPriceNum = numerator iDatum
        , swapUTxOPriceDen = denominator iDatum
        }

    toAvailableAsset :: RawAssetInfo -> AvailableAsset
    toAvailableAsset RawAssetInfo{rawUnit=u,rawQuantity=q} =
      if u == "lovelace"
      then AvailableAsset
             { assetPolicyId = u
             , assetTokenName = ""
             , assetQuantity = q
             }
      else AvailableAsset
             { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
             , assetTokenName = drop 56 u
             , assetQuantity = q
             }