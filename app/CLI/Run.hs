module CLI.Run
(
  runCommand
) where

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

runCreateDatum :: Price -> FilePath -> IO ()
runCreateDatum price file = writeData file price

runCreateSwapRedeemer :: Action -> FilePath -> IO ()
runCreateSwapRedeemer action file = writeData file action

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  CreateSwapScript pkh oa aa file -> runCreateSwapScript pkh oa aa file
  CreateSwapDatum price file -> runCreateDatum price file
  CreateSwapRedeemer action file -> runCreateSwapRedeemer action file
  _ -> return ()