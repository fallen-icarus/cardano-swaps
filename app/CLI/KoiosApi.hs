{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.KoiosApi
(
  queryKoios,
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

import CardanoSwaps (CurrencySymbol(..),TokenName(..))

-------------------------------------------------
-- Data Types for Api Parameters and Responses
-------------------------------------------------
-- | A list of all desired fields from api response schema.
newtype SelectParams = SelectParams { unSelectParams :: [String] }

instance ToHttpApiData SelectParams where
  toQueryParam = T.pack . concat . intersperse "," . unSelectParams

-- | A newtype wrapper around CurrencySymbol from Ledger.Value.
newtype CurrencySymbolParam = CurrencySymbolParam { unCurrencySymbolParam :: CurrencySymbol }

instance ToHttpApiData CurrencySymbolParam where
  toQueryParam = T.pack . init . tail . show . unCurrencySymbol . unCurrencySymbolParam

-- | A newtype wrapper around TokenName from Ledger.Value.
newtype TokenNameParam = TokenNameParam { unTokenNameParam :: TokenName }

instance ToHttpApiData TokenNameParam where
  toQueryParam = T.pack . init . tail . show . unTokenName . unTokenNameParam

-- | The response for the asset address list api.
newtype AssetAddressResponse = AssetAddressResponse { paymentAddress :: String } deriving (Show)

instance FromJSON AssetAddressResponse where
  parseJSON (Object o) = AssetAddressResponse <$> o .: "payment_address"
  parseJSON _ = mzero

-- | The post data type for the address info api.
newtype AddressesPost = AddressesPost { unAddresses :: [String] }

instance ToJSON AddressesPost where
  toJSON (AddressesPost as) = object ["_addresses" .= as]

-- | The response for the address info api
newtype AddressInfoResponse = AddressInfoResponse { utxoSet :: [UtxoSet] } deriving (Show)

instance FromJSON AddressInfoResponse where
  parseJSON (Object o) = AddressInfoResponse <$> o .: "utxo_set"
  parseJSON _ = mempty

-- | The utxo info that comes in the AddressInfo Response
data UtxoSet = UtxoSet
  { txHash :: String
  , txIndex :: Integer
  , value :: String  -- ^ The total lovelace at this utxo.
  , datumHash :: Maybe String
  , inlineDatum :: Maybe InlineDatum
  , referenceScript :: Maybe ReferenceScript
  , assetList :: [Asset]
  } deriving (Show)

instance FromJSON UtxoSet where
  parseJSON (Object o) =
    UtxoSet
      <$> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "value"
      <*> o .: "datum_hash"
      <*> o .: "inline_datum"
      <*> o .: "reference_script"
      <*> o .: "asset_list"

-- | The inline datum info that is part of UtxoSet.
data InlineDatum = InlineDatum { inlineDatumValue :: Map String Value } deriving (Show)

instance FromJSON InlineDatum where
  parseJSON (Object o) = InlineDatum <$> o .: "value"
  parseJSON _ = mzero

-- | The reference script info that is part of UtxoSet.
data ReferenceScript = ReferenceScript { refScriptHash :: String } deriving (Show)

instance FromJSON ReferenceScript where
  parseJSON (Object o) = ReferenceScript <$> o .: "hash"
  parseJSON _ = mzero

-- | The asset info that is part of UtxoSet.
data Asset = Asset
  { assetPolicyID :: String
  , assetName :: String
  , assetQuantity :: String
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
  =    "asset_address_list" 
    :> QueryParam' '[Required] "_asset_policy" CurrencySymbolParam
    :> QueryParam' '[Required] "_asset_name" TokenNameParam
    :> QueryParam' '[Required] "select" SelectParams
    :> Get '[JSON] [AssetAddressResponse]

  :<|> "address_info"
    :> ReqBody '[JSON] AddressesPost
    :> QueryParam' '[Required] "select" SelectParams
    :> Post '[JSON] [AddressInfoResponse]

assetAddressListApi :<|> addressInfoApi = client api
  where
    api :: Proxy KoiosApi
    api = Proxy

-------------------------------------------------
-- Query Koios Function
-------------------------------------------------
queryKoios :: CurrencySymbol -> TokenName -> ClientM [AddressInfoResponse]
queryKoios currSym tokName = do
  addrs <- assetAddressListApi
             (CurrencySymbolParam currSym) 
             (TokenNameParam tokName) 
             (SelectParams ["payment_address"])
  utxos <- addressInfoApi
             (AddressesPost $ take 100 $ map paymentAddress addrs)
             (SelectParams ["utxo_set"])
  return utxos