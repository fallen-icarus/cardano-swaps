{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey (..),
  queryBlockfrost,
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
import qualified Data.Vector as Vector

import CardanoSwaps (CurrencySymbol(..),TokenName(..))

-------------------------------------------------
-- Data Types for Api Parameters and Responses
-------------------------------------------------
-- | The api key for using blockfrost.
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | The asset parameter for asset addresses api
data AssetParam = AssetParam CurrencySymbol TokenName

instance ToHttpApiData AssetParam where
  toQueryParam (AssetParam currSym tokName)
    =  (T.pack $ init $ tail $ show $ unCurrencySymbol currSym)
    <> (T.pack $ init $ tail $ show $ unTokenName tokName)

-- | The response data type for the asset addresses api.
--   It is also used as a param to lookup utxos of the address.
newtype AssetAddress = AssetAddress { unAssetAddress :: String } deriving (Show)

instance FromJSON AssetAddress where
  parseJSON (Object o) = AssetAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData AssetAddress where
  toQueryParam = T.pack . unAssetAddress

-- | The data type for utxo information.
data UtxoSet = UtxoSet
  { txHash :: String
  , outputIndex :: Integer
  , amount :: [AssetInfo]
  , dataHash :: Maybe String
  -- , inlineDatum :: Maybe String
  , referenceScriptHash :: Maybe String
  } deriving (Show)

instance FromJSON UtxoSet where
  parseJSON (Object o) = 
    UtxoSet
      <$> o .: "tx_hash"
      <*> o .: "output_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
      -- <*> o .: "inline_datum"
      <*> o .: "reference_script_hash" 
  
  parseJSON _ = mzero

-- | The assetInfo data type used in UtxoSet
data AssetInfo = AssetInfo
  { unit :: String  -- ^ CurrencySymbol <> TokenName
  , quantity :: String
  } deriving (Show)

instance FromJSON AssetInfo where
  parseJSON (Object o) =
    AssetInfo
      <$> o .: "unit"
      <*> o .: "quantity"
  parseJSON _ = mzero

-- | The inline price datum type used in the datumInfoApi
data InlinePriceDatum = InlinePriceDatum
  { numerator :: Integer
  , denominator :: Integer
  } deriving (Show)

instance FromJSON InlinePriceDatum where
  parseJSON (Object o) =
    InlinePriceDatum
      <$> ((o .: "json_value") >>= (.: "fields") >>= (.: "int") . (Vector.! 0))
      <*> ((o .: "json_value") >>= (.: "fields") >>= (.: "int") . (Vector.! 1))

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" AssetParam
    :> "addresses"
    :> Get '[JSON] [AssetAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" AssetAddress
    :> "utxos"
    :> Get '[JSON] [UtxoSet]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] InlinePriceDatum

assetAddressListApi :<|> addressInfoApi :<|> datumInfoApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Query Blockfrost Function
-------------------------------------------------
queryBlockfrost :: BlockfrostApiKey -> CurrencySymbol -> TokenName -> ClientM InlinePriceDatum
queryBlockfrost apiKey currSym tokName = do
  -- addrs <- assetAddressListApi apiKey (AssetParam currSym tokName)
  -- utxos <- concat <$> mapM (addressInfoApi apiKey) addrs
  -- return utxos
  datumInfoApi apiKey "8d68aabedc6b44e0faec6f678d9eecc1456b8cc395ac2df201cfc867c0181c72"