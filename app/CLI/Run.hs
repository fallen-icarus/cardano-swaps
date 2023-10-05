{-# LANGUAGE TemplateHaskell #-}

module CLI.Run
(
  runCommand
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import Data.FileEmbed

import CardanoSwaps
import CLI.Types
import CLI.Query

preprodParams :: SBS.ByteString
preprodParams = $(embedFile "preprod-params.json")

mainnetParams :: SBS.ByteString
mainnetParams = $(embedFile "mainnet-params.json")

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateDatum d file -> runCreateDatum d file
  CreateSwapRedeemer r file -> writeData file r
  CreateBeaconRedeemer r file -> writeData file r
  BeaconInfo info output -> runBeaconInfo info output
  Query query -> runQuery query
  Submit network api txFile -> 
    runSubmit network api txFile >>= TIO.putStrLn
  ExportParams network output -> case (network,output) of
    (PreProdTestnet,Stdout) -> SBS.putStr preprodParams
    (PreProdTestnet,File file) -> SBS.writeFile file preprodParams
    (Mainnet,Stdout) -> SBS.putStr mainnetParams
    (Mainnet,File file) -> SBS.writeFile file mainnetParams

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  res <- writeScript file $ case script of
    SwapScript -> swapScript
    BeaconPolicy (OfferAsset cfg) -> beaconScript cfg
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err

runCreateDatum :: InternalSwapDatum -> FilePath -> IO ()
runCreateDatum (InternalSwapDatum (OfferAsset offerCfg) (AskAsset askCfg) swapPrice') file = do
  writeData file $ 
    SwapDatum 
      { beaconId = beaconCurrencySymbol offerCfg
      , beaconName = genBeaconName askCfg
      , offerId = fst offerCfg
      , offerName = snd offerCfg
      , askId = fst askCfg
      , askName = snd askCfg
      , swapPrice = swapPrice'
      }

runBeaconInfo :: BeaconInfo -> Output -> IO ()
runBeaconInfo info output = do
  let i = case info of
            PolicyId (OfferAsset offerCfg) -> show $ beaconCurrencySymbol offerCfg
            AssetName (AskAsset askCfg) -> drop 2 $ show $ genBeaconName askCfg
            FullName (OfferAsset offerCfg) (AskAsset askCfg) ->
              show (beaconCurrencySymbol offerCfg) ++ "." ++ drop 2 (show $ genBeaconName askCfg)
  case output of
    Stdout -> Prelude.putStr i
    File file -> Prelude.writeFile file i

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryOwn queryOwn -> runQueryOwn queryOwn
  QueryAll queryAll -> runQueryAll queryAll
  QueryPersonal network api addr format output ->
    runQueryPersonalAddress network api addr >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO 
        Plain -> toPlainOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO

runQueryOwn :: QueryOwn -> IO ()
runQueryOwn queryOwn = case queryOwn of
  QueryOwnSwaps 
    network api addr format output -> 
      runQueryOwnSwaps network api addr >>= 
        case format of
          JSON -> toJSONOutput output
          Pretty -> toPrettyOutput output . (<> hardline) . vsep . map prettySwapUTxO 
          Plain -> toPlainOutput output . (<> hardline) . vsep . map prettySwapUTxO
  QueryOwnSwapsByOffer 
    network api addr (OfferAsset cfg) format output ->
      runQueryOwnSwapsByOffer network api addr (beaconCurrencySymbol cfg) >>= 
        case format of
          JSON -> toJSONOutput output
          Pretty -> toPrettyOutput output . (<> hardline) . vsep . map prettySwapUTxO 
          Plain -> toPlainOutput output . (<> hardline) . vsep . map prettySwapUTxO
  QueryOwnSwapsByTradingPair 
    network api addr (OfferAsset offerCfg) (AskAsset askCfg) format output ->
      let symbol = beaconCurrencySymbol offerCfg
          name = genBeaconName askCfg
      in  runQueryOwnSwapsByTradingPair network api addr symbol name >>= 
            case format of
              JSON -> toJSONOutput output
              Pretty -> toPrettyOutput output . (<> hardline) . vsep . map prettySwapUTxO 
              Plain -> toPlainOutput output . (<> hardline) . vsep . map prettySwapUTxO

runQueryAll :: QueryAll -> IO ()
runQueryAll queryAll = case queryAll of
  QueryAllSwapsByOffer 
    network api (OfferAsset cfg) format output ->
      runQueryAllSwapsByOffer network api (beaconCurrencySymbol cfg) >>= 
        case format of
          JSON -> toJSONOutput output
          Pretty -> toPrettyOutput output . (<> hardline) . vsep . map prettySwapUTxO 
          Plain -> toPlainOutput output . (<> hardline) . vsep . map prettySwapUTxO
  QueryAllSwapsByTradingPair 
    network api offer@(OfferAsset offerCfg) (AskAsset askCfg) format output ->
      let symbol = beaconCurrencySymbol offerCfg
          name = genBeaconName askCfg
      in  runQueryAllSwapsByTradingPair network api symbol name offer >>= 
            case format of
              JSON -> toJSONOutput output
              Pretty -> toPrettyOutput output . (<> hardline) . vsep . map prettySwapUTxO 
              Plain -> toPlainOutput output . (<> hardline) . vsep . map prettySwapUTxO

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
intersperseDoc :: Doc ann -> [Doc ann] -> [Doc ann]
intersperseDoc _ [] = []
intersperseDoc _ (x:[]) = [x]
intersperseDoc i (x:xs) = (x <+> i) : intersperseDoc i xs

toAssetName :: CurrencySymbol -> TokenName -> Text
toAssetName "" _ = "lovelace"
toAssetName cur tok = T.pack (show cur) <> "." <> T.pack (showTokenName tok)

prettySwapUTxO :: SwapUTxO -> Doc AnsiStyle
prettySwapUTxO SwapUTxO{..} = 
  vsep [ (annotate (color Blue) "swap_ref:") <+> 
           (pretty $ swapTxHash <> "#" <> T.pack (show swapOutputIndex))
       , indent 4 $ (annotate (color Green) "address:") <+> pretty swapAddress 
       , indent 4 $ 
           maybe (annotate (color Green) "datum:" <+> "none") prettySwapDatum swapDatum
       , indent 4 $ (annotate (color Green) "assets:") <+> 
           align (fillSep $ intersperseDoc "+" $ map pretty swapValue)
       ]
  where
    prettySwapDatum :: SwapDatum -> Doc AnsiStyle
    prettySwapDatum SwapDatum{..} =
      vsep [ (annotate (color Green) "offer:") <+>
               (pretty $ toAssetName offerId offerName)
           , (annotate (color Green) "ask:") <+>
               (pretty $ toAssetName askId askName)
           , (annotate (color Green) "price:") <+> pretty swapPrice
           ]

prettyPersonalUTxO :: PersonalUTxO -> Doc AnsiStyle
prettyPersonalUTxO PersonalUTxO{..} =
  hsep $ [ pretty personalTxHash
         , "   "
         , pretty personalOutputIndex
         , "      "
         , concatWith (surround " + ") $ map pretty personalValue
         ] <>
         mDatum <>
         mScriptHash
  where
    mDatum :: [Doc AnsiStyle]
    mDatum = 
      maybe [] 
            ( (:[]) 
            . ("+" <+>)
            . annotate (color Green)
            . ("DatumHash" <+>) 
            . pretty
            ) 
            personalDatumHash

    mScriptHash :: [Doc AnsiStyle]
    mScriptHash = 
      maybe [] 
            ( (:[]) 
            . ("+" <+>)
            . annotate (color Blue)
            . ("ScriptHash" <+>) 
            . pretty
            ) 
            personalReferenceScriptHash

personalHeader :: Doc ann
personalHeader = 
  let s = "                           TxHash                                 TxIx        Amount"
  in  pretty s <> 
      hardline <> 
      pretty (replicate (T.length s + 2) '-') <> 
      hardline

toPlainOutput :: Output -> Doc AnsiStyle -> IO ()
toPlainOutput Stdout xs = TIO.putStr $ T.pack $ show $ unAnnotate xs
toPlainOutput (File file) xs = TIO.writeFile file $ T.pack $ show xs

toPrettyOutput :: Output -> Doc AnsiStyle -> IO ()
toPrettyOutput Stdout xs = putDoc xs
toPrettyOutput (File file) xs = 
  TIO.writeFile file $ renderStrict $ layoutPretty defaultLayoutOptions xs

toJSONOutput :: (ToJSON a) => Output -> [a] -> IO ()
toJSONOutput Stdout xs = LBS.putStr $ encode xs
toJSONOutput (File file) xs = LBS.writeFile file $ encode xs
