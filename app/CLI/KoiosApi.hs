{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.KoiosApi
(
  queryAllSwapsByTradingPair,
  queryAllSwapsByOffer,
  queryOwnSwaps,
  queryOwnSwapsByTradingPair,
  queryOwnSwapsByOffer
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (isJust)

import CLI.Types
import CardanoSwaps

-- | Koios supports querying addresses only by policy ids while blockfrost does not. This means
-- only Koios can be used for offer-based queries.

-------------------------------------------------
-- Core Types
-------------------------------------------------
instance ToHttpApiData CurrencySymbol where
  toQueryParam currSym = T.pack $ show currSym

instance ToHttpApiData TokenName where
  toQueryParam tokName = T.pack $ drop 2 $ show tokName

-- | An address that contains a beacon.
-- The response type of the beaconAddressList api.
newtype BeaconAddress = BeaconAddress { unBeaconAddress :: String } deriving (Show)

instance FromJSON BeaconAddress where
  parseJSON (Object o) = BeaconAddress <$> o .: "payment_address"
  parseJSON _ = mzero

newtype AddressList = AddressList { unAddressList :: [BeaconAddress] } deriving (Show)

instance ToJSON AddressList where
  toJSON (AddressList as) = object [ "_addresses" .= map unBeaconAddress as ]

data RawBeaconInfo = RawBeaconInfo
  { rawAddress :: String
  , rawUTxOInfo :: [RawUTxOInfo]
  } deriving (Show)

instance FromJSON RawBeaconInfo where
  parseJSON (Object o) = RawBeaconInfo <$> o .: "address" <*> o .: "utxo_set"
  parseJSON _ = mzero

data RawUTxOInfo = RawUTxOInfo
  { rawTxHash :: String
  , rawOutputIndex :: Integer
  , rawLovelaceValue :: String
  , rawDatum :: Maybe Value
  , rawAssetInfo :: [Asset]
  } deriving (Show)

instance FromJSON RawUTxOInfo where
  parseJSON (Object o) =
    RawUTxOInfo
      <$> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "value"
      <*> (o .: "inline_datum" >>= (.: "value"))
      <*> o .: "asset_list"
  parseJSON _ = mzero

data RawAssetInfo = RawAssetInfo
  { rawPolicyId :: String
  , rawTokenName :: String
  , rawQuantity :: String
  } deriving (Show)

instance FromJSON Asset where
  parseJSON (Object o) =
    Asset
      <$> o .: "policy_id"
      <*> o .: "asset_name"
      <*> o .: "quantity"
  parseJSON _ = mzero

-------------------------------------------------
-- Koios Api
-------------------------------------------------
type KoiosApi
  =    "asset_addresses"
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "_asset_name" TokenName
     :> QueryParam' '[Required] "select" Text
     :> Get '[JSON] [BeaconAddress]

  :<|> "policy_asset_addresses"
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "select" Text
     :> Get '[JSON] [BeaconAddress]

  :<|> "address_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] AddressList
     :> Post '[JSON] [RawBeaconInfo]

pairBeaconAddressListApi :<|> offerBeaconAddressListApi :<|> addressUTxOsApi = client api
  where
    api :: Proxy KoiosApi
    api = Proxy

-------------------------------------------------
-- Koios Query Functions
-------------------------------------------------
queryAllSwapsByTradingPair :: CurrencySymbol -> TokenName -> AssetConfig -> ClientM [SwapUTxO]
queryAllSwapsByTradingPair beaconSym beaconTokName AssetConfig{..} = do
  let beacon = (show beaconSym, drop 2 $ show beaconTokName)
      offer = (show assetId, drop 2 $ show assetName)
  addrs <- pairBeaconAddressListApi beaconSym beaconTokName "payment_address"
  utxos <- addressUTxOsApi "address,utxo_set" (AddressList addrs)  
  return $ concatMap (convertTradingPairToSwapUTxO beacon offer) utxos

queryAllSwapsByOffer :: CurrencySymbol -> ClientM [SwapUTxO]
queryAllSwapsByOffer beaconSym = do
  addrs <- offerBeaconAddressListApi beaconSym "payment_address"
  utxos <- addressUTxOsApi "address,utxo_set" (AddressList addrs)  
  return $ concatMap (convertOfferToSwapUTxO (show beaconSym)) utxos

queryOwnSwaps :: SwapAddress -> ClientM [SwapUTxO]
queryOwnSwaps (SwapAddress addr) = do
  utxos <- addressUTxOsApi "address,utxo_set" (AddressList [BeaconAddress addr])
  return $ concatMap convertAllToSwapUTxO utxos

queryOwnSwapsByTradingPair :: SwapAddress -> CurrencySymbol -> TokenName -> ClientM [SwapUTxO]
queryOwnSwapsByTradingPair (SwapAddress addr) beaconSym beaconTokName = do
  let beacon = (show beaconSym, drop 2 $ show beaconTokName)
  utxos <- addressUTxOsApi "address,utxo_set" (AddressList [BeaconAddress addr])  
  return $ concatMap (convertAllTradingPairToSwapUTxO beacon) utxos

queryOwnSwapsByOffer :: SwapAddress -> CurrencySymbol -> ClientM [SwapUTxO]
queryOwnSwapsByOffer (SwapAddress addr) beaconSym = do
  utxos <- addressUTxOsApi "address,utxo_set" (AddressList [BeaconAddress addr])  
  return $ concatMap (convertOfferToSwapUTxO (show beaconSym)) utxos

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
hasAsset :: (String,String) -> [Asset] -> Bool
hasAsset (oSym,oName) = isJust . find (\(Asset sym name _) -> sym == oSym && name == oName)

hasBeacon :: String -> [Asset] -> Bool
hasBeacon oSym = isJust . find (\(Asset sym _ _ ) -> sym == oSym)

-- | This will return only the UTxOs for the target trading pair with the offer asset..
convertTradingPairToSwapUTxO :: (String,String) -> (String,String) -> RawBeaconInfo -> [SwapUTxO]
convertTradingPairToSwapUTxO _ _ (RawBeaconInfo _ []) = []
convertTradingPairToSwapUTxO beacon offer@(oSym,_) (RawBeaconInfo addr (RawUTxOInfo{..}:xs))
  -- | If lovelace is offered, only check for the beacon.
  | oSym == "" && hasAsset beacon rawAssetInfo = info : next
  -- | Otherwise check for both the beacon and the offered asset.
  | hasAsset beacon rawAssetInfo && hasAsset offer rawAssetInfo = info : next
  -- | If the above two checks failed, this UTxO is for another trading pair.
  | otherwise = next
  where
    swapDatum = join $ fmap decodeDatum rawDatum
    next = convertTradingPairToSwapUTxO beacon offer (RawBeaconInfo addr xs)
    info = SwapUTxO
      { address = addr
      , txIx = rawTxHash <> "#" <> show rawOutputIndex
      , value = (Asset "" "" rawLovelaceValue) : rawAssetInfo
      , datum = swapDatum
      }

-- | This will return any swap UTxOs with the specified offer asset.
convertOfferToSwapUTxO :: String -> RawBeaconInfo -> [SwapUTxO]
convertOfferToSwapUTxO _ (RawBeaconInfo _ []) = []
convertOfferToSwapUTxO beaconSym (RawBeaconInfo addr (RawUTxOInfo{..}:xs))
  -- | If the UTxO has one of the beacon's, it is should be returned.
  | hasBeacon beaconSym rawAssetInfo = info : next
  -- | Otherwise, this UTxO is for another trading pair.
  | otherwise = next
  where
    swapDatum = join $ fmap decodeDatum rawDatum
    next = convertOfferToSwapUTxO beaconSym (RawBeaconInfo addr xs)
    info = SwapUTxO
      { address = addr
      , txIx = rawTxHash <> "#" <> show rawOutputIndex
      , value = (Asset "" "" rawLovelaceValue) : rawAssetInfo
      , datum = swapDatum
      }

-- | This will return all swap UTxOs.
convertAllToSwapUTxO :: RawBeaconInfo -> [SwapUTxO]
convertAllToSwapUTxO (RawBeaconInfo _ []) = []
convertAllToSwapUTxO (RawBeaconInfo addr (RawUTxOInfo{..}:xs)) = info : next
  where
    swapDatum = join $ fmap decodeDatum rawDatum
    next = convertAllToSwapUTxO (RawBeaconInfo addr xs)
    info = SwapUTxO
      { address = addr
      , txIx = rawTxHash <> "#" <> show rawOutputIndex
      , value = (Asset "" "" rawLovelaceValue) : rawAssetInfo
      , datum = swapDatum
      }

-- | This will return all the UTxOs for the target trading pair.
convertAllTradingPairToSwapUTxO :: (String,String) -> RawBeaconInfo -> [SwapUTxO]
convertAllTradingPairToSwapUTxO _ (RawBeaconInfo _ []) = []
convertAllTradingPairToSwapUTxO beacon (RawBeaconInfo addr (RawUTxOInfo{..}:xs))
  -- | Check for the beacon.
  | hasAsset beacon rawAssetInfo = info : next
  -- | If the above check failed, this UTxO is for another trading pair.
  | otherwise = next
  where
    swapDatum = join $ fmap decodeDatum rawDatum
    next = convertAllTradingPairToSwapUTxO beacon (RawBeaconInfo addr xs)
    info = SwapUTxO
      { address = addr
      , txIx = rawTxHash <> "#" <> show rawOutputIndex
      , value = (Asset "" "" rawLovelaceValue) : rawAssetInfo
      , datum = swapDatum
      }