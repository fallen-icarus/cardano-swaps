{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAllSwapsByTradingPair,
  runQueryOwnSwapsByTradingPair,
  runQueryOwnSwaps,
  runQueryAllSwapsByOffer,
  runQueryOwnSwapsByOffer
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.KoiosApi as Koios
import CLI.Types
import CardanoSwaps

{- Offer based queries are only supported by Koios. -}

runQueryAllSwapsByTradingPair :: Network 
                              -> ApiEndpoint 
                              -> CurrencySymbol 
                              -> TokenName 
                              -> AssetConfig
                              -> IO [SwapUTxO]
runQueryAllSwapsByTradingPair network api beaconSym beaconTokName offer = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryAllSwapsByTradingPair beaconSym beaconTokName offer) env
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      runClientM (Blockfrost.queryAllSwapsByTradingPair apiKey' beaconSym beaconTokName offer) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSwapsByTradingPair :: Network 
                              -> ApiEndpoint 
                              -> SwapAddress
                              -> CurrencySymbol 
                              -> TokenName 
                              -> IO [SwapUTxO]
runQueryOwnSwapsByTradingPair network api addr beaconSym beaconTokName = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwapsByTradingPair addr beaconSym beaconTokName) env
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      runClientM (Blockfrost.queryOwnSwapsByTradingPair apiKey' beaconSym beaconTokName addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSwaps :: Network 
                 -> ApiEndpoint 
                 -> SwapAddress
                 -> IO [SwapUTxO]
runQueryOwnSwaps network api addr = do
  manager' <- newManager tlsManagerSettings
  res <- case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      runClientM (Koios.queryOwnSwaps addr) env
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      runClientM (Blockfrost.queryOwnSwaps apiKey' addr) env
  case res of
    Right r -> return r
    Left err -> throw err

-- | Only Koios supported.
runQueryAllSwapsByOffer :: Network -> CurrencySymbol -> IO [SwapUTxO]
runQueryAllSwapsByOffer PreProdTestnet beaconSym = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
  res <- runClientM (Koios.queryAllSwapsByOffer beaconSym) env
  case res of
    Right r -> return r
    Left err -> throw err

-- | Only Koios supported.
runQueryOwnSwapsByOffer :: Network -> SwapAddress -> CurrencySymbol -> IO [SwapUTxO]
runQueryOwnSwapsByOffer PreProdTestnet addr beaconSym = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
  res <- runClientM (Koios.queryOwnSwapsByOffer addr beaconSym) env
  case res of
    Right r -> return r
    Left err -> throw err