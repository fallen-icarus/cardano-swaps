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
  CreateSpendingRedeemer r file -> runCreateSpendingRedeemer r file
  CreateMintingRedeemer r file -> runCreateMintingRedeemer r file
  BeaconInfo info output -> runBeaconInfo info output
  Query query -> runQuery query
  Submit network api txFile -> 
    runSubmit network api txFile >>= LBS.putStr . encode
  EvaluateTx network api txFile -> 
    runEvaluateTx network api txFile >>= LBS.putStr . encode
  ExportParams network output -> runExportParams network output

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  res <- writeScript file $ case script of
    OneWaySwapScript -> oneWaySwapScript
    OneWayBeaconPolicy -> oneWayBeaconScript
    TwoWaySwapScript -> twoWaySwapScript
    TwoWayBeaconPolicy -> twoWayBeaconScript
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err

runCreateDatum :: InternalDatum -> FilePath -> IO ()
runCreateDatum 
  (InternalOneWaySwapDatum (OfferAsset offerCfg) (AskAsset askCfg) swapPrice' mPrev) file = do
    writeData file $ OneWaySwapDatum 
      { oneWayBeaconId = oneWayBeaconCurrencySymbol
      , oneWayPairBeacon = genUnsortedPairBeaconName offerCfg askCfg
      , oneWayOfferId = fst offerCfg
      , oneWayOfferName = snd offerCfg
      , oneWayOfferBeacon = uncurry genOfferBeaconName offerCfg
      , oneWayAskId = fst askCfg
      , oneWayAskName = snd askCfg
      , oneWaySwapPrice = swapPrice'
      , oneWayPrevInput = mPrev
      }
runCreateDatum 
  (InternalTwoWaySwapDatum (TwoWayPair (asset1,asset2)) forwardPrice' reversePrice' mPrev) file = do
    writeData file $ TwoWaySwapDatum 
        { twoWayBeaconId = twoWayBeaconCurrencySymbol
        , twoWayPairBeacon = genUnsortedPairBeaconName asset1 asset2
        , twoWayAsset1Id = fst asset1
        , twoWayAsset1Name = snd asset1
        , twoWayAsset1Beacon = uncurry genOfferBeaconName asset1
        , twoWayAsset2Id = fst asset2
        , twoWayAsset2Name = snd asset2
        , twoWayAsset2Beacon = uncurry genOfferBeaconName asset2
        , twoWayForwardPrice = forwardPrice'
        , twoWayReversePrice = reversePrice'
        , twoWayPrevInput = mPrev
        }

runCreateSpendingRedeemer :: SpendingRedeemer -> FilePath -> IO ()
runCreateSpendingRedeemer (OneWaySpendingRedeemer r) file = writeData file r
runCreateSpendingRedeemer (TwoWaySpendingRedeemer r) file = case r of
  KnownTwoWaySwapRedeemer r' -> writeData file r'
  UnknownTwoWaySwapRedeemer offer ask -> writeData file $ getRequiredSwapDirection offer ask

runCreateMintingRedeemer :: MintingRedeemer -> FilePath -> IO ()
runCreateMintingRedeemer (OneWayMintingRedeemer r) file = writeData file r
runCreateMintingRedeemer (TwoWayMintingRedeemer r) file = writeData file r

runBeaconInfo :: BeaconInfo -> Output -> IO ()
runBeaconInfo info output = case output of
    Stdout -> Prelude.putStr name
    File file -> Prelude.writeFile file name
  where
    name :: String
    name = 
      case info of
        OneWayPolicyId -> 
          show oneWayBeaconCurrencySymbol
        OneWayOfferBeaconAssetName (OfferAsset cfg) -> 
          drop 2 $ show $ uncurry genOfferBeaconName cfg
        OneWayPairBeaconAssetName (OfferAsset offerCfg, AskAsset askCfg) ->
          drop 2 $ show $ genUnsortedPairBeaconName offerCfg askCfg
        TwoWayPolicyId -> 
          show twoWayBeaconCurrencySymbol
        TwoWayOfferBeaconAssetName cfg -> 
          drop 2 $ show $ uncurry genOfferBeaconName cfg
        TwoWayPairBeaconAssetName (TwoWayPair (assetX,assetY)) ->
          drop 2 $ show $ genSortedPairBeaconName assetX assetY

runExportParams :: Network -> Output -> IO ()
runExportParams network output = case (network,output) of
  (PreProdTestnet,Stdout) -> SBS.putStr preprodParams
  (PreProdTestnet,File file) -> SBS.writeFile file preprodParams
  (Mainnet,Stdout) -> SBS.putStr mainnetParams
  (Mainnet,File file) -> SBS.writeFile file mainnetParams

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryAllSwaps queryAll -> runQueryAll queryAll
  QueryOwnSwaps queryOwn -> runQueryOwn queryOwn
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

runQueryAll :: QueryAll -> IO ()
runQueryAll queryAll = case queryAll of
  QueryAllSwapsByTradingPair 
    network api offer@(OfferAsset o) ask@(AskAsset a) format output -> do
      results <- runQueryAllSwapsByTradingPair network api offer ask
      let reqDirection = if o < a then Reverse else Forward
      case format of
        JSON -> toJSONOutput output results
        Pretty -> 
          toPrettyOutput output $ (<> hardline) $ vsep $ map (prettySwapUTxO reqDirection) results
        Plain -> 
          toPlainOutput output $ (<> hardline) $ vsep $ map (prettySwapUTxO reqDirection) results
  QueryAllSwapsByOffer 
    network api offer format output ->
      runQueryAllSwapsByOffer network api offer >>= 
        case format of
          JSON -> toJSONOutput output
          Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None) 
          Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)

runQueryOwn :: QueryOwnSwaps -> IO ()
runQueryOwn queryOwn = case queryOwn of
  QueryOwnOneWaySwaps 
    network api addr format output -> 
      runQueryOwnSwaps network api addr >>= 
        case format of
          JSON -> toJSONOutput output
          Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None) 
          Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)
  QueryOwnOneWaySwapsByOffer 
    network api addr (OfferAsset cfg) format output ->
      runQueryOwnSwapsByBeacon 
        network api addr oneWayBeaconCurrencySymbol (uncurry genOfferBeaconName cfg) >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None) 
        Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)
  QueryOwnOneWaySwapsByTradingPair 
    network api addr (OfferAsset offerCfg) (AskAsset askCfg) format output ->
      runQueryOwnSwapsByBeacon 
        network api addr oneWayBeaconCurrencySymbol (genUnsortedPairBeaconName offerCfg askCfg) >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None) 
        Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)
  QueryOwnTwoWaySwaps 
    network api addr format output -> 
      runQueryOwnSwaps network api addr >>= 
        case format of
          JSON -> toJSONOutput output
          Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None) 
          Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)
  QueryOwnTwoWaySwapsByOffer 
    network api addr cfg format output ->
      runQueryOwnSwapsByBeacon 
        network api addr twoWayBeaconCurrencySymbol (uncurry genOfferBeaconName cfg) >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None) 
        Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)
  QueryOwnTwoWaySwapsByTradingPair 
    network api addr (TwoWayPair (assetX,assetY)) format output ->
      runQueryOwnSwapsByBeacon 
        network api addr twoWayBeaconCurrencySymbol (genSortedPairBeaconName assetX assetY) >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)
        Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettySwapUTxO None)

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

data TargetDirection = None | Forward | Reverse deriving (Eq)

prettySwapUTxO :: TargetDirection -> SwapUTxO -> Doc AnsiStyle
prettySwapUTxO target SwapUTxO{..} = 
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
    prettySwapDatum (OneWayDatum OneWaySwapDatum{..}) =
      vsep [ (annotate (color Green) "type:") <+> pretty @Text "one-way"
           , (annotate (color Green) "offer:") <+>
               (pretty $ toAssetName oneWayOfferId oneWayOfferName)
           , (annotate (color Green) "ask:") <+>
               (pretty $ toAssetName oneWayAskId oneWayAskName)
           , (annotate (color Green) "price:") <+> 
               annotate (color Magenta) (pretty oneWaySwapPrice)
           ]
    prettySwapDatum (TwoWayDatum TwoWaySwapDatum{..}) =
      vsep [ (annotate (color Green) "type:") <+> pretty @Text "two-way"
           , (annotate (color Green) "asset1:") <+>
               (pretty $ toAssetName twoWayAsset1Id twoWayAsset1Name)
           , (annotate (color Green) "asset2:") <+>
               (pretty $ toAssetName twoWayAsset2Id twoWayAsset2Name)
           , (annotate (color Green) "forward_price:") <+> 
               if target == Forward then
                 annotate (color Magenta) (pretty twoWayForwardPrice)
               else pretty twoWayForwardPrice
           , (annotate (color Green) "reverse_price:") <+>
               if target == Reverse then
                 annotate (color Magenta) (pretty twoWayReversePrice)
               else pretty twoWayReversePrice
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

