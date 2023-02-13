module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CardanoSwaps
import CLI.Types
import CLI.QuerySwaps

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  SwapCmd swapCmd -> runSwapCmd swapCmd
  BeaconCmd beaconCmd -> runBeaconCmd beaconCmd
  QueryAvailableSwaps askedAsset offeredAsset network output ->
    runQueryAvailableSwaps askedAsset offeredAsset network output

runQueryAvailableSwaps :: Asset -> Asset -> Network -> Output -> IO ()
runQueryAvailableSwaps askedAsset offeredAsset network output = do
    avail <- runQuery beaconSym (tupledAsset offeredAsset) network
    case output of
      Stdout -> BL.putStr $ encode avail
      File file -> BL.writeFile file $ encodePretty avail
  where
    beaconSym :: CurrencySymbol
    beaconSym = beaconSymbol $ assets2SwapConfig askedAsset offeredAsset

runSwapCmd :: SwapCmd -> IO ()
runSwapCmd swapCmd = case swapCmd of
    ExportSwapScript askedAsset offeredAsset file -> exportScript askedAsset offeredAsset file
    CreateSwapDatum datumPrice file -> createDatum datumPrice file
    CreateSwapRedeemer action file -> createRedeemer action file
  where
    createRedeemer :: Action -> FilePath -> IO ()
    createRedeemer action file = do
      writeData file action
      putStrLn "Swap redeemer created successfully."

    createDatum :: DatumPrice -> FilePath -> IO ()
    createDatum datumPrice file = do
      writeData file $ SwapDatum {swapPrice = convert2price datumPrice, swapBeacon = Nothing}
      putStrLn "Swap datum created successfully."

    exportScript :: Asset -> Asset -> FilePath -> IO ()
    exportScript askedAsset offeredAsset file = do
      let swapConfig = assets2SwapConfig askedAsset offeredAsset
      res <- writeScript file $ swapValidatorScript swapConfig
      case res of
        Right _ -> putStrLn "Swap script exported successfully."
        Left err -> putStrLn $ "There was an error: " <> show err

runBeaconCmd :: BeaconCmd -> IO ()
runBeaconCmd beaconCmd = case beaconCmd of
    ExportBeaconPolicy askedAsset offeredAsset file -> exportPolicy askedAsset offeredAsset file
    CreateBeaconDatum askedAsset offeredAsset datumPrice file -> 
      createDatum askedAsset offeredAsset datumPrice file
    CreateBeaconRedeemer r file -> createRedeemer r file
  where
    createRedeemer :: BeaconRedeemer -> FilePath -> IO ()
    createRedeemer r file = do
      writeData file r
      putStrLn "Beacon redeemer created successfully."

    createDatum :: Asset -> Asset -> DatumPrice -> FilePath -> IO ()
    createDatum askedAsset offeredAsset datumPrice file = do
      let beaconSymbol' = beaconSymbol $ assets2SwapConfig askedAsset offeredAsset
      writeData file $ SwapDatum 
        { swapPrice = convert2price datumPrice
        , swapBeacon = Just beaconSymbol'
        }
      putStrLn "Special datum for beacon created successfully."

    exportPolicy :: Asset -> Asset -> FilePath -> IO ()
    exportPolicy askedAsset offeredAsset file = do
      let swapConfig = assets2SwapConfig askedAsset offeredAsset
      res <- writeScript file $ beaconScript swapConfig
      case res of
        Right _ -> putStrLn "Beacon policy exported successfully."
        Left err -> putStrLn $ "There was an error: " <> show err

-------------------------------------------------
-- Helpers
-------------------------------------------------
tupledAsset :: Asset -> (CurrencySymbol,TokenName)
tupledAsset Ada = (adaSymbol,adaToken)
tupledAsset (Asset currSym tokName) = (currSym,tokName)

assets2SwapConfig :: Asset -> Asset -> SwapConfig
assets2SwapConfig askedAsset offeredAsset = SwapConfig
  { swapAsk = tupledAsset askedAsset
  , swapOffer = tupledAsset offeredAsset
  }

convert2price :: DatumPrice -> Price
convert2price datumPrice = case datumPrice of
  NewDatum price' -> 
    if price' <= fromGHC (fromInteger (0 :: Integer))
    then error "Price is not > 0."
    else price' 
  WeightedPrice xs -> calcWeightedPrice xs