{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAvailableSwaps,
  runQueryOwnUTxOs
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.KoiosApi as Koios
import CLI.Types
import CardanoSwaps

runQueryAvailableSwaps :: Network 
                       -> ApiEndpoint 
                       -> CurrencySymbol 
                       -> (CurrencySymbol,TokenName) 
                       -> IO [SwapUTxO]
runQueryAvailableSwaps network api beaconSym target = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      res <- runClientM (Koios.queryAvailableSwaps beaconSym target) env
      case res of
        Right r -> return r
        Left err -> throw err
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryAvailableSwaps apiKey' beaconSym target) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryOwnUTxOs :: Network -> ApiEndpoint -> String -> IO [SwapUTxO]
runQueryOwnUTxOs network api addr = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v0")
      res <- runClientM (Koios.queryOwnUTxOs addr) env
      case res of
        Right r -> return r
        Left err -> throw err
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryOwnUTxOs apiKey' addr) env
      case res of
        Right r -> return r
        Left err -> throw err