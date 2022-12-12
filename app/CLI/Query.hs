{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  Network (..),
  BlockfrostApiKey (..),
  runQuery,
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Control.Monad
import qualified Data.Text as T
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

import CardanoSwaps (CurrencySymbol(..),TokenName(..))

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

-- | The response format of the Koios asset address list api.
newtype AssetAddressResponse = AssetAddressResponse { paymentAddress :: String } deriving (Show)

instance FromJSON AssetAddressResponse where
  parseJSON (Object o) = AssetAddressResponse <$> o .: "payment_address"
  parseJSON _ = mzero

-- | The post data type for koios address info api.
newtype Addresses = Addresses { unAddresses :: [String] }

instance ToJSON Addresses where
  toJSON (Addresses as) = object ["_addresses" .= as]

newtype AddressInfoResponse = AddressInfoResponse { utxoSet :: [UtxoSet] } deriving (Show)

instance FromJSON AddressInfoResponse where
  parseJSON (Object o) = AddressInfoResponse <$> o .: "utxo_set"
  parseJSON _ = mempty

data UtxoSet = UtxoSet
  { txHash :: String
  , txIndex :: Integer
  -- , blockHeight :: String
  -- , blockTime :: String
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
      -- <*> o .: "block_height"
      -- <*> o .: "block_time"
      <*> o .: "value"
      <*> o .: "datum_hash"
      <*> o .: "inline_datum"
      <*> o .: "reference_script"
      <*> o .: "asset_list"

data InlineDatum = InlineDatum
  { inlineDatumValue :: Map String Value
  -- , inlineDatumBytes :: String
  } deriving (Show)

instance FromJSON InlineDatum where
  parseJSON (Object o) = 
    InlineDatum 
      <$> o .: "value"
      -- <*> o .: "bytes"

  parseJSON _ = mzero

data ReferenceScript = ReferenceScript
  { refScriptHash :: String
  -- , refScriptSize :: Integer
  -- , refScriptType :: String
  -- , refScriptBytes :: String
  -- , refScriptValue :: Map String Value
  } deriving (Show)

instance FromJSON ReferenceScript where
  parseJSON (Object o) = 
    ReferenceScript 
      <$> o .: "hash"
      -- <*> o .: "size"
      -- <*> o .: "type"
      -- <*> o .: "bytes"
      -- <*> o .: "value"

  parseJSON _ = mzero

data Asset = Asset
  { assetPolicyID :: String
  , assetName :: String
  -- , assetFingerprint :: String
  , assetQuantity :: String
  } deriving (Show)

instance FromJSON Asset where
  parseJSON (Object o) = 
    Asset 
      <$> o .: "policy_id"
      <*> o .: "asset_name"
      -- <*> o .: "fingerprint"
      <*> o .: "quantity"

  parseJSON _ = mzero

data Network
  -- | Uses the Koios REST api. 
  = Mainnet
  -- | Koios REST api does not support the PreProduction Testnet.
  --   Until they do, Blockfrost will be used for querying the PreProduction Testnet.
  | PreProdTestnet BlockfrostApiKey

newtype BlockfrostApiKey = BlockfrostApiKey String

type KoiosApi 
  =    "asset_address_list" 
    :> QueryParam' '[Required] "_asset_policy" CurrencySymbolParam
    :> QueryParam' '[Required] "_asset_name" TokenNameParam
    :> QueryParam' '[Required] "select" SelectParams
    :> Get '[JSON] [AssetAddressResponse]

  :<|> "address_info"
    :> ReqBody '[JSON] Addresses
    :> QueryParam' '[Required] "select" SelectParams
    :> Post '[JSON] [AddressInfoResponse]


assetAddressListApi :<|> addressInfoApi = client api
  where
    api :: Proxy KoiosApi
    api = Proxy

-- -- | Get all addresses that contain the native token with the designated policy and name
-- assetAddressListApi :: CurrencySymbolParam -> TokenNameParam -> SelectParams -> ClientM [AssetAddressResponse]
-- assetAddressListApi = client api
--   where
--     api :: Proxy KoiosAssetAddressApi
--     api = Proxy

testAssetAddressListApi :: ClientM [AssetAddressResponse]
testAssetAddressListApi = assetAddressListApi 
  (CurrencySymbolParam $ CurrencySymbol "750900e4999ebe0d58f19b634768ba25e525aaf12403bfe8fe130501") 
  (TokenNameParam $ TokenName "424f4f4b")
  (SelectParams ["payment_address"])

testAddressInfoApi :: ClientM [AddressInfoResponse]
testAddressInfoApi = addressInfoApi (Addresses ["addr1qy2jt0qpqz2z2z9zx5w4xemekkce7yderz53kjue53lpqv90lkfa9sgrfjuz6uvt4uqtrqhl2kj0a9lnr9ndzutx32gqleeckv"])
  (SelectParams ["utxo_set"])

runQuery :: IO ()
runQuery = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
  res <- runClientM testAddressInfoApi env
  case res of
    Right res' -> print res'
    Left err -> putStrLn $ "Error: " <> show err