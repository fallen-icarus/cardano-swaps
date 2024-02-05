{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module CLI.Query
  (
    runQueryAllSwapsByOffer
  , runQueryAllSwapsByAsk
  , runQueryAllSwapsByTradingPair
  , runQueryOwnSwaps
  , runQueryOwnSwapsByBeacon
  , runQueryPersonalAddress
  , runSubmit
  , runEvaluateTx
  ) where

import Servant.Client 
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (decode,Value)

import CLI.Query.Koios as Koios
import CLI.Types
import CardanoSwaps.Utils

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

runQueryAllSwapsByAsk
  :: Network 
  -> Endpoint 
  -> AskAsset
  -> IO [SwapUTxO]
runQueryAllSwapsByAsk network api ask = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAllSwapsByAsk ask) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAllSwapsByAsk ask) env
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

runSubmit :: Network -> Endpoint -> FilePath -> IO Value
runSubmit network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err

runEvaluateTx :: Network -> Endpoint -> FilePath -> IO Value
runEvaluateTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err
