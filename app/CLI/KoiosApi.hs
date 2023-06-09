{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.KoiosApi
(
  queryAvailableSwaps,
  queryOwnUTxOs
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
     :> QueryParam' '[Required] "select" Text
     :> Get '[JSON] [BeaconAddress]

  :<|> "address_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] AddressList
     :> Post '[JSON] [RawBeaconInfo]

beaconAddressListApi :<|> swappableUTxOsApi = client api
  where
    api :: Proxy KoiosApi
    api = Proxy

-------------------------------------------------
-- Koios Query Functions
-------------------------------------------------
queryAvailableSwaps :: CurrencySymbol -> (CurrencySymbol,TokenName) -> ClientM [SwapUTxO]
queryAvailableSwaps beaconSym (oSym,oName) = do
  addrs <- beaconAddressListApi beaconSym "payment_address"
  utxos <- swappableUTxOsApi "address,utxo_set" (AddressList addrs)
  return $ concatMap (convertToSwapUTxO (show oSym, drop 2 $ show oName)) utxos

queryOwnUTxOs :: String -> ClientM [SwapUTxO]
queryOwnUTxOs addr = do
  utxos <- swappableUTxOsApi "address,utxo_set" (AddressList [BeaconAddress addr])
  return $ concatMap convertToOwnUTxO utxos

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
hasAsset :: (String,String) -> [Asset] -> Bool
hasAsset (oSym,oName) = isJust . find (\(Asset sym name _) -> sym == oSym && name == oName) 

-- | This only returns UTxOs with the target asset, a SwapPrice datum, and a price > 0.
convertToSwapUTxO :: (String,String) -> RawBeaconInfo -> [SwapUTxO]
convertToSwapUTxO _ (RawBeaconInfo _ []) = []
convertToSwapUTxO t@(oSym,_) (RawBeaconInfo addr (RawUTxOInfo{..}:xs)) =
    if oSym == "" then
      case swapDatum of
        Just (SwapPrice price') -> 
          if price' > unsafeRatio 0 1
          then info : convertToSwapUTxO t (RawBeaconInfo addr xs)
          else convertToSwapUTxO t (RawBeaconInfo addr xs)
        _ -> convertToSwapUTxO t (RawBeaconInfo addr xs)
    else if hasAsset t rawAssetInfo then
      case swapDatum of
        Just (SwapPrice price') -> 
          if price' > unsafeRatio 0 1
          then info : convertToSwapUTxO t (RawBeaconInfo addr xs)
          else convertToSwapUTxO t (RawBeaconInfo addr xs)
        _ -> convertToSwapUTxO t (RawBeaconInfo addr xs)
    else convertToSwapUTxO t (RawBeaconInfo addr xs)
  where
    swapDatum = join $ fmap decodeDatum rawDatum
    info = SwapUTxO
      { address = addr
      , txIx = rawTxHash <> "#" <> show rawOutputIndex
      , value = (Asset "" "" rawLovelaceValue) : rawAssetInfo
      , datum = swapDatum
      }

-- | Returns all UTxOs at address.
convertToOwnUTxO :: RawBeaconInfo -> [SwapUTxO]
convertToOwnUTxO (RawBeaconInfo _ []) = []
convertToOwnUTxO (RawBeaconInfo addr (RawUTxOInfo{..}:xs)) =
    info : convertToOwnUTxO (RawBeaconInfo addr xs)
  where
    swapDatum = join $ fmap decodeDatum rawDatum
    info = SwapUTxO
      { address = addr
      , txIx = rawTxHash <> "#" <> show rawOutputIndex
      , value = (Asset "" "" rawLovelaceValue) : rawAssetInfo
      , datum = swapDatum
      }