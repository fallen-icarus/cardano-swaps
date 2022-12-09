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

data Network
  -- | Uses the Koios REST api. 
  = Mainnet
  -- | Koios REST api does not support the PreProduction Testnet.
  --   Until they do, Blockfrost will be used for querying the PreProduction Testnet.
  | PreProdTestnet BlockfrostApiKey

newtype BlockfrostApiKey = BlockfrostApiKey String

data AssetAddress = AssetAddress
  { paymentAddress :: String
  , quantity :: String
  } deriving (Eq,Ord,Show)

instance FromJSON AssetAddress where
  parseJSON (Object o) = 
    AssetAddress
      <$> o .: "payment_address"
      <*> o .: "quantity"
  
  parseJSON _ = mzero

type KoiosAssetAddressApi 
  =  "asset_address_list" 
  :> QueryParam' '[Required] "_asset_policy" String
  :> QueryParam' '[Required] "_asset_name" String
  :> Get '[JSON] [AssetAddress]

koiosAssetAddressApi :: Proxy KoiosAssetAddressApi
koiosAssetAddressApi = Proxy

assetAddressList :: String -> String -> ClientM [AssetAddress]
assetAddressList = client koiosAssetAddressApi

queryAssetAddressList :: ClientM [AssetAddress]
queryAssetAddressList = assetAddressList "750900e4999ebe0d58f19b634768ba25e525aaf12403bfe8fe130501" "424f4f4b"

runQuery :: IO ()
runQuery = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM queryAssetAddressList (mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0"))
  case res of
    Right res' -> print $ take 3 res'
    Left err -> putStrLn $ "Error: " <> show err