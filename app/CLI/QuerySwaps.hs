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

import CardanoSwaps (CurrencySymbol(..),TokenName(..))
import CLI.KoiosApi
import CLI.BlockfrostApi as Frost
import CLI.Types

runQuery :: QueryAsset -> QueryAsset -> Network -> IO [AvailableSwap]
runQuery (QueryAsset (bSym,bTok)) (QueryAsset (tSym,tTok)) network = do
  manager' <- newManager tlsManagerSettings
  case network of
    PreProdTestnet apiKey -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
          beaconId = Frost.BeaconId $ bSym <> bTok
          targetId = Frost.TargetId $ tSym <> tTok
      res <- runClientM (queryBlockfrost apiKey' beaconId targetId) env
      case res of
        Right res' -> return res'
        Left err -> throw err
    Mainnet -> return []

  -- let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v0")
  --     currSym = CurrencySymbol "750900e4999ebe0d58f19b634768ba25e525aaf12403bfe8fe130501"
  --     tokName = TokenName "424f4f4b"
  -- res <- runClientM (queryKoios currSym tokName) env


  -- case res of
  --   Right res' -> print res'
  --   Left err -> putStrLn $ "Error: " <> show err