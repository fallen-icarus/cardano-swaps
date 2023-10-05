{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CLI.Query
  (
    runQueryOwnSwaps
  , runQueryOwnSwapsByOffer
  , runQueryOwnSwapsByTradingPair
  , runQueryAllSwapsByOffer
  , runQueryAllSwapsByTradingPair
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

runQueryOwnSwaps :: Network -> QueryEndpoint -> UserAddress -> IO [SwapUTxO]
runQueryOwnSwaps network api addr = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwaps addr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwaps addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSwapsByOffer 
  :: Network 
  -> QueryEndpoint 
  -> UserAddress 
  -> CurrencySymbol 
  -> IO [SwapUTxO]
runQueryOwnSwapsByOffer network api addr beaconSym = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwapsByOffer addr beaconSym) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwapsByOffer addr beaconSym) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSwapsByTradingPair 
  :: Network 
  -> QueryEndpoint 
  -> UserAddress
  -> CurrencySymbol 
  -> TokenName 
  -> IO [SwapUTxO]
runQueryOwnSwapsByTradingPair network api addr beaconSym beaconTokName = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwapsByTradingPair addr beaconSym beaconTokName) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwapsByTradingPair addr beaconSym beaconTokName) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryAllSwapsByOffer :: Network -> QueryEndpoint -> CurrencySymbol -> IO [SwapUTxO]
runQueryAllSwapsByOffer network api beaconSym = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryAllSwapsByOffer beaconSym) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
      runClientM (Koios.queryAllSwapsByOffer beaconSym) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryAllSwapsByTradingPair 
  :: Network 
  -> QueryEndpoint 
  -> CurrencySymbol 
  -> TokenName 
  -> OfferAsset
  -> IO [SwapUTxO]
runQueryAllSwapsByTradingPair network api beaconSym beaconTokName (OfferAsset offer) = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryAllSwapsByTradingPair beaconSym beaconTokName offer) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
      runClientM (Koios.queryAllSwapsByTradingPair beaconSym beaconTokName offer) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryPersonalAddress :: Network -> QueryEndpoint -> UserAddress -> IO [PersonalUTxO]
runQueryPersonalAddress network api addr = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryPersonalAddress addr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
      runClientM (Koios.queryPersonalAddress addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runSubmit :: Network -> QueryEndpoint -> FilePath -> IO Text
runSubmit network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
          runClientM (Koios.submitTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
          runClientM (Koios.submitTx tx) env
      case res of
        Right r -> return r
        Left err -> throw err
