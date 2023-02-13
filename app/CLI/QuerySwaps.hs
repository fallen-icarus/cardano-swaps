{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.QuerySwaps
(
  runQuery
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi
import CLI.Types
import CardanoSwaps (CurrencySymbol,TokenName)

-- | Takes the beacon symbol, the target asset, and the desired network to query the relevant
-- off-chain api endpoint.
runQuery :: CurrencySymbol -> (CurrencySymbol,TokenName) -> Network -> IO [AvailableSwap]
runQuery beaconSym offeredAsset network = do
  manager' <- newManager tlsManagerSettings
  case network of
    PreProdTestnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconSym offeredAsset) env
      case res of
        Right r -> return r
        Left err -> throw err
    Mainnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-mainnet.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconSym offeredAsset) env
      case res of
        Right r -> return r
        Left err -> throw err