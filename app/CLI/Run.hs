{-# LANGUAGE TemplateHaskell #-}

module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.FileEmbed

import CardanoSwaps
import CLI.Types

blueprintsFile :: ByteString
blueprintsFile = $(embedFile "aiken/plutus.json")

blueprints :: Blueprints
blueprints = parseBlueprints $ BL.fromStrict blueprintsFile

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateSwapDatum d file -> runCreateSwapDatum d file
  CreateSwapRedeemer r file -> writeData file r
  CreateBeaconRedeemer r file -> writeData file r

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  let script' = case script of
        SwapScript cfg -> unValidatorScript $ spendingValidator $ genScripts cfg blueprints
        BeaconPolicy cfg -> unMintingPolicyScript $ beaconPolicy $ genScripts cfg blueprints
  res <- writeScript file script'
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err

runCreateSwapDatum :: Datum -> FilePath -> IO ()
runCreateSwapDatum (SwapDatum d) file = writeData file d
runCreateSwapDatum (WeightedPrice ps) file = writeData file $ SwapPrice $ calcWeightedPrice ps