{-# LANGUAGE TemplateHaskell #-}

module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CardanoSwaps
import CLI.Types
import CLI.Query

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateDatum d file -> runCreateDatum d file
  CreateSwapRedeemer r file -> writeData file r
  CreateBeaconRedeemer r file -> writeData file r
  BeaconInfo info output -> runBeaconInfo info output
  QueryBeacons query -> runQuery query

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  let script' = case script of
        SwapScript -> swapScript
        BeaconPolicy cfg -> beaconScript cfg
  res <- writeScript file script'
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err

runCreateDatum :: SwapDatumInfo -> FilePath -> IO ()
runCreateDatum (SwapDatumInfo offerConfig askConfig swapPrice') file = do
  let beaconSym = beaconCurrencySymbol offerConfig
  writeData file $ 
    SwapDatum 
      { beaconId = beaconSym
      , beaconName = genBeaconName askConfig
      , offerId = fst offerConfig
      , offerName = snd offerConfig
      , askId = fst askConfig
      , askName = snd askConfig
      , swapPrice = swapPrice'
      }

runBeaconInfo :: BeaconInfo -> Output -> IO ()
runBeaconInfo info output = do
  let i = case info of
            PolicyId offerCfg -> show $ beaconCurrencySymbol offerCfg
            AssetName askCfg -> drop 2 $ show $ genBeaconName askCfg
  case output of
    Stdout -> putStr i
    File file -> writeFile file i

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryAllSwapsByTradingPair network api offerCfg askCfg output -> do
    let beaconSym = beaconCurrencySymbol offerCfg
        beaconTokName = genBeaconName askCfg
    runQueryAllSwapsByTradingPair network api beaconSym beaconTokName offerCfg >>= toOutput output

  QueryOwnSwapsByTradingPair network api addr offerCfg askCfg output -> do
    let beaconSym = beaconCurrencySymbol offerCfg
        beaconTokName = genBeaconName askCfg
    runQueryOwnSwapsByTradingPair network api addr beaconSym beaconTokName >>= toOutput output

  QueryAllSwapsByOffer network offerCfg output -> do
    let beaconSym = beaconCurrencySymbol offerCfg
    runQueryAllSwapsByOffer network beaconSym >>= toOutput output

  QueryOwnSwapsByOffer network addr offerCfg output -> do
    let beaconSym = beaconCurrencySymbol offerCfg
    runQueryOwnSwapsByOffer network addr beaconSym >>= toOutput output

  QueryOwnSwaps network api addr output -> do
    runQueryOwnSwaps network api addr >>= toOutput output

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toOutput :: (ToJSON a) => Output -> a -> IO ()
toOutput output xs = case output of
  Stdout -> BL.putStr $ encode xs
  File file -> BL.writeFile file $ encodePretty xs
