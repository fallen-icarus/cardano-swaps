{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.QuerySwaps
(
  runQuery,
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi
import CLI.Types

runQuery :: QueryAsset -> QueryAsset -> Network -> IO [AvailableSwap]
runQuery (QueryAsset (bSym,bTok)) (QueryAsset (tSym,tTok)) network = do
  manager' <- newManager tlsManagerSettings
  let beaconId = BeaconId $ bSym <> bTok
      targetId = TargetId $ tSym <> tTok
  case network of
    PreProdTestnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (queryBlockfrost apiKey' beaconId targetId) env
      case res of
        Right res' -> return res'
        Left err -> throw err
    Mainnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-mainnet.blockfrost.io" 443 "api/v0") 
          apiKey' = BlockfrostApiKey apiKey   
      res <- runClientM (queryBlockfrost apiKey' beaconId targetId) env
      case res of
        Right res' -> return res'
        Left err -> throw err