module CLI.Run
(
  runCommand,
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BSU

import CardanoSwaps
import CLI.Types
import CLI.QuerySwaps

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  SwapScript swapCmd -> runSwapScriptCmd swapCmd
  StakingScript stakingCmd -> runStakingScriptCmd stakingCmd
  Beacon beaconCmd -> runBeaconCmd beaconCmd
  QueryAvailableSwaps rOfferedAsset rAskedAsset network output ->
    runQueryAvailableSwaps rOfferedAsset rAskedAsset network output

runQueryAvailableSwaps :: RawAsset -> RawAsset -> Network -> Output -> IO ()
runQueryAvailableSwaps rOfferedAsset rAskedAsset network output = do
    avail <- runQuery beaconQueryAsset (rawToQuery rOfferedAsset) network
    case output of
      StdOut -> print avail
      File file -> return ()
  where
    rawToQuery :: RawAsset -> QueryAsset
    rawToQuery RawAda = QueryAsset ("lovelace","")
    rawToQuery (RawAsset sym tok) = QueryAsset (BSU.toString sym, BSU.toString tok)

    beaconTokenName :: String
    beaconTokenName 
      = drop 2 
      $ show 
      $ genBeaconTokenName (rawAssetInfo rOfferedAsset) (rawAssetInfo rAskedAsset)

    beaconQueryAsset :: QueryAsset
    beaconQueryAsset = QueryAsset (show beaconSymbol, beaconTokenName)

runBeaconCmd :: BeaconCmd -> IO ()
runBeaconCmd beaconCmd = case beaconCmd of
    GenerateBeaconTokenName rOfferedAsset rAskedAsset output ->
      runGenTokenName rOfferedAsset rAskedAsset output
    ExportBeaconPolicyId output -> runExportBeaconPolicyId output
    CreateBeaconRedeemer beaconRedeemer file -> runCreateRedeemer beaconRedeemer file
    CreateBeaconDatum file -> runCreateDatum file
    ExportBeaconPolicyScript file -> runExportPolicy file
    ExportBeaconVaultScript file -> runExportVaultScript file
  
  where
    runGenTokenName :: RawAsset -> RawAsset -> Output -> IO ()
    runGenTokenName rOfferedAsset rAskedAsset output = do
      let name = drop 2 
               $ show 
               $ genBeaconTokenName (rawAssetInfo rOfferedAsset) (rawAssetInfo rAskedAsset)
      case output of
        StdOut -> putStr name
        File file -> writeFile file name >> putStrLn "Beacon token name generated successfully."

    runExportBeaconPolicyId :: Output -> IO ()
    runExportBeaconPolicyId output = do
      let policyId = show beaconSymbol
      case output of
        StdOut -> putStr $ show beaconSymbol
        File file -> writeFile file policyId >> putStrLn "Beacon policy id exported successfully."

    runCreateRedeemer :: BeaconRedeemer -> FilePath -> IO ()
    runCreateRedeemer r file = do
      writeData file r
      putStrLn "Beacon redeemer created successfully."

    runCreateDatum :: FilePath -> IO ()
    runCreateDatum file = do
      writeData file beaconSymbol
      putStrLn "Beacon vault datum created successfully."
    
    runExportPolicy :: FilePath -> IO ()
    runExportPolicy file = do
      res <- writeScript file beaconScript
      case res of
        Right _ -> putStrLn "Beacon policy script exported successfully."
        Left err -> putStrLn $ "There was an error: " <> show err

    runExportVaultScript :: FilePath -> IO ()
    runExportVaultScript file = do
      res <- writeScript file beaconVaultScript
      case res of
        Right _ -> putStrLn "Beacon vault script exported successfully."
        Left err -> putStrLn $ "There was an error: " <> show err

runStakingScriptCmd :: StakingScriptCmd -> IO ()
runStakingScriptCmd stakingCmd = case stakingCmd of
    CreateStakingScript pkh offeredAsset askedAsset file -> 
      runCreateScript pkh offeredAsset askedAsset file
    CreateStakingRedeemer file -> runCreateRedeemer file

  where
    runCreateScript :: PaymentPubKeyHash -> Maybe Asset -> Maybe Asset -> FilePath -> IO ()
    runCreateScript pkh offeredAsset askedAsset file = do
      let stakeConfig = StakeConfig
                     { stakeOwner = pkh
                     , stakeOfferedAsset = assetInfo <$> offeredAsset
                     , stakeAskedAsset = assetInfo <$> askedAsset
                     }
      res <- writeScript file $ stakingScript stakeConfig
      case res of
        Right _ -> putStrLn "Staking script created successfully."
        Left err -> putStrLn $ "There was an error: " <> show err

    runCreateRedeemer :: FilePath -> IO ()
    runCreateRedeemer file = do
      writeData file ()
      putStrLn "Staking redeemer file created successfully."

runSwapScriptCmd :: SwapScriptCmd -> IO ()
runSwapScriptCmd swapCmd = case swapCmd of
    CreateSwapScript pkh offeredAsset askedAsset file -> 
      runCreateScript pkh offeredAsset askedAsset file
    CreateSwapDatum datumInfo file -> runCreateDatum datumInfo file
    CreateSwapRedeemer action file -> runCreateRedeemer action file

  where
    runCreateScript :: PaymentPubKeyHash -> Asset -> Asset -> FilePath -> IO ()
    runCreateScript pkh offeredAsset askedAsset file = do
      let basicInfo = BasicInfo
            { owner = pkh
            , offerAsset = assetInfo offeredAsset
            , askAsset = assetInfo askedAsset
            }
      res <- writeScript file $ swapScript basicInfo
      case res of
        Right _ -> putStrLn "Swap script created successfully."
        Left err -> putStrLn $ "There was an error: " <> show err

    runCreateDatum :: SwapDatumInfo -> FilePath -> IO ()
    runCreateDatum datumInfo file = case datumInfo of
      SwapDatum price ->
        if price > fromGHC (toRational (0 :: Integer))
        then do
          writeData file price
        else putStrLn "Invalid swap datum. Price must be greater than 0."
      SwapDatumUtxos utxoFile -> do
        utxos <- BL.readFile utxoFile
        case decode utxos of
          Nothing -> putStrLn "There was an error parsing the utxos file."
          Just uis -> do
            writeData file $ calcWeightedPrice uis
            putStrLn "Swap datum created successfully."
      SwapDatumUtxosTemplate -> do
        BL.writeFile file $ encodePretty template
        putStrLn "Template file created successfully."
    
    runCreateRedeemer :: Action -> FilePath -> IO ()
    runCreateRedeemer action file = do
      writeData file action
      putStrLn "Swap redeemer created successfully."

template :: [UtxoPriceInfo]
template =
  [ UtxoPriceInfo { utxoAmount = 100, priceNumerator = 1, priceDenominator = 1 }
  , UtxoPriceInfo { utxoAmount = 200, priceNumerator = 2, priceDenominator = 1 }
  ]

assetInfo :: Asset -> (CurrencySymbol,TokenName)
assetInfo Ada = (adaSymbol,adaToken)
assetInfo (Asset currSym tokName) = (currSym,tokName)

rawAssetInfo :: RawAsset -> (BS.ByteString,BS.ByteString)
rawAssetInfo RawAda = (BS.empty,BS.empty)
rawAssetInfo (RawAsset currSym tokName) = (currSym,tokName)