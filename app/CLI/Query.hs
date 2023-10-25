{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CLI.Query
  (
    runQueryAllSwapsByOffer
  , runQueryAllSwapsByTradingPair
  , runQueryOwnSwaps
  , runQueryOwnSwapsByBeacon
  , runQueryPersonalAddress
  , runSubmit
  ) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (decode)

import CLI.Query.Koios as Koios
import CLI.Types
import CardanoSwaps

runQueryAllSwapsByTradingPair 
  :: Network 
  -> Endpoint 
  -> OfferAsset
  -> AskAsset
  -> IO [SwapUTxO]
runQueryAllSwapsByTradingPair network api offer ask = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAllSwapsByTradingPair offer ask) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAllSwapsByTradingPair offer ask) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryAllSwapsByOffer
  :: Network 
  -> Endpoint 
  -> OfferAsset
  -> IO [SwapUTxO]
runQueryAllSwapsByOffer network api offer = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAllSwapsByOffer offer) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAllSwapsByOffer offer) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSwaps
  :: Network 
  -> Endpoint 
  -> UserAddress
  -> IO [SwapUTxO]
runQueryOwnSwaps network api addr = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryOwnSwaps addr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryOwnSwaps addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSwapsByBeacon
  :: Network 
  -> Endpoint 
  -> UserAddress
  -> CurrencySymbol
  -> TokenName
  -> IO [SwapUTxO]
runQueryOwnSwapsByBeacon network api addr beaconId beaconName = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryOwnSwapsByBeacon addr beaconId beaconName) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryOwnSwapsByBeacon addr beaconId beaconName) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryPersonalAddress :: Network -> Endpoint -> UserAddress -> IO [PersonalUTxO]
runQueryPersonalAddress network api addr = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runSubmit :: Network -> Endpoint -> FilePath -> IO Text
runSubmit network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
          runClientM (Koios.submitTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
          runClientM (Koios.submitTx tx) env
      case res of
        Right r -> return r
        Left err -> throw err
