module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CardanoSwaps
import CLI.Parsers

assetInfo :: Asset -> (CurrencySymbol,TokenName)
assetInfo Ada = (adaSymbol,adaToken)
assetInfo (Asset currSym tokName) = (currSym,tokName)

runCreateSwapScript :: PaymentPubKeyHash -> Asset -> Asset -> FilePath -> IO ()
runCreateSwapScript pkh oa aa file = do
  let basicInfo = BasicInfo 
                    { owner = pkh
                    , offerAsset = assetInfo oa
                    , askAsset = assetInfo aa
                    }
  res <- writeScript file $ swapScript basicInfo
  case res of
    Right _ -> putStrLn "Swap script created successfully."
    Left err -> putStrLn $ "There was an error: " <> show err

runCreateDatum :: SwapDatumInfo -> FilePath -> IO ()
runCreateDatum d file = case d of
     SwapDatum price -> do
       writeData file price
       putStrLn "Datum file created successfully."
     SwapDatumUtxos utxoFile -> do
       utxos <- BL.readFile utxoFile
       case decode utxos of
         Nothing -> putStrLn "There was an error parsing the utxos file."
         Just uis -> do
           writeData file $ snd $ calcWeightedPrice uis
           putStrLn "Datum file created successfully."
     SwapDatumUtxosTemplate -> do
       BL.writeFile file $ encodePretty template
       putStrLn "Template file created successfully."
  where
    template :: [UtxoPriceInfo]
    template =
      [ UtxoPriceInfo { utxoAmount = 100, priceNumerator = 1, priceDenominator = 1 }
      , UtxoPriceInfo { utxoAmount = 200, priceNumerator = 2, priceDenominator = 1 }
      ]

runCreateSwapRedeemer :: Action -> FilePath -> IO ()
runCreateSwapRedeemer action file = do
  writeData file action
  putStrLn "Redeemer file created successfully."

runCreateStakingScript :: PaymentPubKeyHash -> Maybe Asset -> Maybe Asset -> FilePath -> IO ()
runCreateStakingScript pkh moa maa file = do
  let stakeConfig = StakeConfig
                     { stakeOwner = pkh
                     , stakeOfferedAsset = assetInfo <$> moa
                     , stakeAskedAsset = assetInfo <$> maa
                     }
  res <- writeScript file $ stakingScript stakeConfig
  case res of
    Right _ -> putStrLn "Staking script created successfully."
    Left err -> putStrLn $ "There was an error: " <> show err

runCreateBeaconTokenName :: Asset -> Asset -> FilePath -> IO ()
runCreateBeaconTokenName oa aa file = do
  writeFile file $ show $ genBeaconTokenName (assetInfo oa) (assetInfo aa)
  putStrLn "Beacon token name generated successfully."

runCreateStakingRedeemer :: FilePath -> IO ()
runCreateStakingRedeemer file = do
  writeData file ()
  putStrLn "Redeemer file created successfully."

runCreateBeaconRedeemer :: BeaconRedeemer -> FilePath -> IO ()
runCreateBeaconRedeemer r file = do
  writeData file r
  putStrLn "Redeemer file created successfully."

runCreateBeaconDatum :: FilePath -> IO ()
runCreateBeaconDatum file = do
  writeData file beaconSymbol
  putStrLn "Beacon vault datum created successfully."

runAdvancedCommands :: AdvancedOption -> FilePath -> IO ()
runAdvancedCommands o file = case o of
  BeaconPolicyId -> do
    writeFile file $ show beaconSymbol
    putStrLn "Beacon policy id save successfully."
  BeaconPolicy -> do
    res <- writeScript file beaconScript
    case res of
      Right _ -> putStrLn "Beacon policy saved successfully."
      Left err -> putStrLn $ "There was an error: " <> show err
  BeaconVaultScript -> do
    res <- writeScript file beaconVaultScript
    case res of
      Right _ -> putStrLn "Beacon vault script saved successfully."
      Left err -> putStrLn $ "There was an error: " <> show err

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  CreateSwapScript pkh oa aa file -> runCreateSwapScript pkh oa aa file
  CreateSwapDatum d file -> runCreateDatum d file
  CreateSwapRedeemer action file -> runCreateSwapRedeemer action file
  CreateStakingScript pkh moa maa file -> runCreateStakingScript pkh moa maa file
  CreateStakingRedeemer file -> runCreateStakingRedeemer file
  CreateBeaconRedeemer r file -> runCreateBeaconRedeemer r file
  CreateBeaconTokenName oa aa file -> runCreateBeaconTokenName oa aa file
  CreateBeaconDatum file -> runCreateBeaconDatum file
  Advanced advancedOptions file -> runAdvancedCommands advancedOptions file
  _ -> return ()