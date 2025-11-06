module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative

import CardanoSwaps.Utils
import qualified CardanoSwaps.OneWaySwap as OneWay
import qualified CardanoSwaps.TwoWaySwap as TwoWay
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "scripts" $
      info parseExportScript $ progDesc "Export a protocol script."
  , command "datums" $
      info parseCreateDatum $ progDesc "Create a datum for the protocol."
  , command "spending-redeemers" $
      info  parseCreateSpendingRedeemer $ progDesc "Create a spending redeemer."
  , command "beacon-redeemers" $
      info parseCreateMintingRedeemer $ progDesc "Create a redeemer for the beacon policy."
  , command "beacon-info" $
      info parseBeaconInfo $ progDesc "Calculate a beacon policy id or asset name."
  , command "time" $
      info pTimeCommand $ progDesc "Work with time formats."
  , command "query" $
      info parseQuery $ progDesc "Query the blockchain."
  , command "submit" $
      info pSubmit $ progDesc "Submit a transaction to the blockchain."
  , command "evaluate-tx" $
      info pEvaluateTx $ progDesc "Estimate script execution units for a transaction."
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = hsubparser $ mconcat
    [ command "one-way" $
        info parseOneWayExportScript $ progDesc "Export a one-way swap script."
    , command "two-way" $
        info parseTwoWayExportScript $ progDesc "Export a two-way swap script."
    ]

parseOneWayExportScript :: Parser Command
parseOneWayExportScript = hsubparser $ mconcat
    [ command "beacon-script" $
        info pExportPolicy $ progDesc "Export the beacon script for one-way swaps."
    , command "swap-script" $
        info pExportSwap $ progDesc "Export the swap script for one-way swaps."
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = 
      ExportScript 
        <$> pure OneWayBeaconScript
        <*> pOutputFile
    
    pExportSwap :: Parser Command
    pExportSwap = 
      ExportScript 
        <$> pure OneWaySwapScript
        <*> pOutputFile

parseTwoWayExportScript :: Parser Command
parseTwoWayExportScript = hsubparser $ mconcat
    [ command "beacon-script" $
        info pExportPolicy $ progDesc "Export the beacon script for two-way swaps."
    , command "swap-script" $
        info pExportSwap $ progDesc "Export the swap script for two-way swaps."
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = 
      ExportScript 
        <$> pure TwoWayBeaconScript
        <*> pOutputFile
    
    pExportSwap :: Parser Command
    pExportSwap = 
      ExportScript 
        <$> pure TwoWaySwapScript
        <*> pOutputFile

-------------------------------------------------
-- CreateDatum Parser
-------------------------------------------------
parseCreateDatum :: Parser Command
parseCreateDatum = hsubparser $ mconcat
    [ command "one-way" $
        info pCreateOneWayDatum $ progDesc "Create a one-way swap datum."
    , command "two-way" $
        info pCreateTwoWayDatum $ progDesc "Create a two-way swap datum."
    ]

pCreateOneWayDatum :: Parser Command
pCreateOneWayDatum = CreateDatum <$> pInternalOneWaySwapDatum <*> pOutputFile
  where
    pInternalOneWaySwapDatum :: Parser InternalDatum
    pInternalOneWaySwapDatum = 
      InternalOneWaySwapDatum 
        <$> (OfferAsset <$> pAssetConfig "offer")
        <*> (AskAsset <$> pAssetConfig "ask")
        <*> pPrice "offer"
        <*> pPrevInput
        <*> pSwapExpiration

pCreateTwoWayDatum :: Parser Command
pCreateTwoWayDatum = CreateDatum <$> pInternalTwoWaySwapDatum <*> pOutputFile
  where
    pInternalTwoWaySwapDatum :: Parser InternalDatum
    pInternalTwoWaySwapDatum = 
      InternalTwoWaySwapDatum 
        <$> ((,) <$> pAssetConfig "first" <*> pAssetConfig "second")
        <*> pPrice "first"
        <*> pPrice "second"
        <*> pPrevInput
        <*> pSwapExpiration

-------------------------------------------------
-- Spending Redeemer Parser
-------------------------------------------------
parseCreateSpendingRedeemer :: Parser Command
parseCreateSpendingRedeemer = hsubparser $ mconcat
    [ command "one-way" $
        info pCreateOneWaySpendingRedeemer $ progDesc "Create a one-way swap spending redeemer."
    , command "two-way" $
        info pCreateTwoWaySpendingRedeemer $ progDesc "Create a two-way swap spending redeemer."
    ]

pCreateOneWaySpendingRedeemer :: Parser Command
pCreateOneWaySpendingRedeemer = 
    CreateSpendingRedeemer 
      <$> (OneWaySpendingRedeemer <$> (pClose <|> pUpdateMint <|> pUpdateStake <|> pSwap))
      <*> pOutputFile
  where
    pClose :: Parser OneWay.SwapRedeemer
    pClose = flag' OneWay.SpendWithMint
      (  long "close"
      <> help "Close swap(s)."
      )

    pUpdateMint :: Parser OneWay.SwapRedeemer
    pUpdateMint = flag' OneWay.SpendWithMint
      (  long "update-with-mint"
      <> help "Update swap(s) when minting/burning beacons in tx."
      )

    pUpdateStake :: Parser OneWay.SwapRedeemer
    pUpdateStake = flag' OneWay.SpendWithStake
      (  long "update-with-stake"
      <> help "Update swap(s) when NOT minting/burning beacons in tx."
      )


    pSwap :: Parser OneWay.SwapRedeemer
    pSwap = flag' OneWay.Swap
      (  long "swap" 
      <> help "Swap with assets at a swap address."
      )

pCreateTwoWaySpendingRedeemer :: Parser Command
pCreateTwoWaySpendingRedeemer = 
    CreateSpendingRedeemer 
      <$> (TwoWaySpendingRedeemer <$> (pKnownTwoWayRedeemer <|> pUnknownTwoWaySwapRedeemer))
      <*> pOutputFile
  where
    pKnownTwoWayRedeemer :: Parser InternalTwoWaySwapRedeemer
    pKnownTwoWayRedeemer = 
      KnownTwoWaySwapRedeemer 
        <$> (pClose <|> pUpdateMint <|> pUpdateStake <|> pTakeAsset1 <|> pTakeAsset2)

    pUnknownTwoWaySwapRedeemer :: Parser InternalTwoWaySwapRedeemer
    pUnknownTwoWaySwapRedeemer = 
      UnknownTwoWaySwapRedeemer 
        <$> (OfferAsset <$> pAssetConfig "offer") 
        <*> (AskAsset <$> pAssetConfig "ask")

    pClose :: Parser TwoWay.SwapRedeemer
    pClose = flag' TwoWay.SpendWithMint
      (  long "close"
      <> help "Close swap(s)."
      )

    pUpdateMint :: Parser TwoWay.SwapRedeemer
    pUpdateMint = flag' TwoWay.SpendWithMint
      (  long "update-with-mint"
      <> help "Update swap(s) when minting/burning beacons in tx."
      )

    pUpdateStake :: Parser TwoWay.SwapRedeemer
    pUpdateStake = flag' TwoWay.SpendWithStake
      (  long "update-with-stake"
      <> help "Update swap(s) when NOT minting/burning beacons in tx."
      )

    pTakeAsset1 :: Parser TwoWay.SwapRedeemer
    pTakeAsset1 = flag' TwoWay.TakeAsset1
      (  long "take-asset1" 
      <> help "Take asset1 from a swap."
      )

    pTakeAsset2 :: Parser TwoWay.SwapRedeemer
    pTakeAsset2 = flag' TwoWay.TakeAsset2
      (  long "take-asset2" 
      <> help "Take asset2 from a swap."
      )

-------------------------------------------------
-- Beacon Redeemer Parser
-------------------------------------------------
parseCreateMintingRedeemer :: Parser Command
parseCreateMintingRedeemer = hsubparser $ mconcat
    [ command "one-way" $
        info pCreateOneWayBeaconRedeemer $ progDesc "Create a one-way swap beacon redeemer."
    , command "two-way" $
        info pCreateTwoWayBeaconRedeemer $ progDesc "Create a two-way swap beacon redeemer."
    ]

pCreateOneWayBeaconRedeemer :: Parser Command
pCreateOneWayBeaconRedeemer = 
    CreateMintingRedeemer
      <$> (pMint <|> pStake <|> pRegister)
      <*> pOutputFile
  where
    pMint :: Parser MintingRedeemer
    pMint = flag' (OneWayMintingRedeemer OneWay.CreateOrCloseSwaps)
      (  long "mint-or-burn"
      <> help "Mint/burn the beacons for a swap."
      )

    pStake :: Parser MintingRedeemer
    pStake = flag' (OneWayMintingRedeemer OneWay.UpdateSwaps)
      (  long "update-only"
      <> help "Update swaps without minting/burning."
      )

    pRegister :: Parser MintingRedeemer
    pRegister = flag' (OneWayMintingRedeemer OneWay.RegisterBeaconScript)
      (  long "register"
      <> help "Register the one-way beacon script."
      )

pCreateTwoWayBeaconRedeemer :: Parser Command
pCreateTwoWayBeaconRedeemer = 
    CreateMintingRedeemer
      <$> (pMint <|> pStake <|> pRegister)
      <*> pOutputFile
  where
    pMint :: Parser MintingRedeemer
    pMint = flag' (TwoWayMintingRedeemer TwoWay.CreateOrCloseSwaps)
      (  long "mint-or-burn"
      <> help "Mint/burn the beacons for a swap."
      )

    pStake :: Parser MintingRedeemer
    pStake = flag' (TwoWayMintingRedeemer TwoWay.UpdateSwaps)
      (  long "update-only"
      <> help "Update swaps without minting/burning."
      )

    pRegister :: Parser MintingRedeemer
    pRegister = flag' (TwoWayMintingRedeemer TwoWay.RegisterBeaconScript)
      (  long "register"
      <> help "Register the two-way beacon script."
      )

-------------------------------------------------
-- Beacon Info Parser
-------------------------------------------------
parseBeaconInfo :: Parser Command
parseBeaconInfo = hsubparser $ mconcat
    [ command "one-way" $
        info pOneWayBeaconInfo $ progDesc "Calculate the name for a one-way beacon."
    , command "two-way" $
        info pTwoWayBeaconInfo $ progDesc "Calculate the name for a two-way beacon."
    ]

pOneWayBeaconInfo :: Parser Command
pOneWayBeaconInfo = hsubparser $ mconcat
    [ command "policy-id" $
        info pPolicyId $ progDesc "Calculate the one-way beacon policy id."
    , command "offer-beacon" $
        info pOfferName $ progDesc "Calculate the one-way offer beacon token name."
    , command "ask-beacon" $
        info pAskName $ progDesc "Calculate the one-way ask beacon token name."
    , command "pair-beacon" $
        info pPairName $ progDesc "Calculate the one-way trading pair beacon token name."
    ]
  where
    pPolicyId :: Parser Command
    pPolicyId = BeaconInfo <$> pure OneWayPolicyId <*> pOutput

    pOfferName :: Parser Command
    pOfferName = 
      BeaconInfo
        <$> (OneWayOfferBeaconName <$> (OfferAsset <$> pAssetConfig "offer"))
        <*> pOutput

    pAskName :: Parser Command
    pAskName = 
      BeaconInfo
        <$> (OneWayAskBeaconName <$> (AskAsset <$> pAssetConfig "ask"))
        <*> pOutput

    pPairName :: Parser Command
    pPairName = 
      BeaconInfo
        <$> ( fmap OneWayPairBeaconName . (,) 
                <$> (OfferAsset <$> pAssetConfig "offer") <*> (AskAsset <$> pAssetConfig "ask")
            )
        <*> pOutput

pTwoWayBeaconInfo :: Parser Command
pTwoWayBeaconInfo = hsubparser $ mconcat
    [ command "policy-id" $
        info pPolicyId $ progDesc "Calculate the two-way beacon policy id."
    , command "asset-beacon" $
        info pAssetName $ progDesc "Calculate the two-way asset beacon token name."
    , command "pair-beacon" $
        info pPairName $ progDesc "Calculate the two-way trading pair beacon token name."
    ]
  where
    pPolicyId :: Parser Command
    pPolicyId = BeaconInfo <$> pure TwoWayPolicyId <*> pOutput

    pAssetName :: Parser Command
    pAssetName = 
      BeaconInfo
        <$> ( TwoWayAssetBeaconName 
                <$> (pAssetConfig "first" <|> pAssetConfig "second")
            )
        <*> pOutput

    pPairName :: Parser Command
    pPairName = 
      BeaconInfo
        <$> ( fmap TwoWayPairBeaconName . (,) 
                <$> pAssetConfig "first" <*> pAssetConfig "second"
            )
        <*> pOutput

-------------------------------------------------
-- Time Parser
-------------------------------------------------
pTimeCommand :: Parser Command
pTimeCommand = hsubparser $ mconcat
    [ command "convert-time" $
        info pConvertTime $ progDesc "Convert POSIXTime <---> Slot."
    , command "round-to-min" $
        info pRoundTime $ progDesc "Round POSIXTime to nearest minute."
    ]

pConvertTime :: Parser Command
pConvertTime = Time <$> pConvert
  where
    pConvert :: Parser Time
    pConvert = ConvertTime <$> (pPOSIXTime <|> pSlot) <*> pNetwork

    pPOSIXTime :: Parser ConvertTime
    pPOSIXTime = POSIXTimeToSlot . POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Convert POSIX time (in milliseconds) to slot number."
      )

    pSlot :: Parser ConvertTime
    pSlot = SlotToPOSIXTime . Slot <$> option auto
      (  long "slot"
      <> metavar "INT"
      <> help "Convert slot number to POSIX time."
      )

pRoundTime :: Parser Command
pRoundTime = Time <$> pRound
  where
    pRound :: Parser Time
    pRound = RoundToMinute <$> pPOSIXTime

    pPOSIXTime :: Parser POSIXTime
    pPOSIXTime = POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Round POSIX time (in milliseconds) to nearest minute."
      )

-------------------------------------------------
-- Submit Parser
-------------------------------------------------
pSubmit :: Parser Command
pSubmit = 
  Submit 
    <$> pNetwork
    <*> pEndpoint
    <*> pTxFile

-------------------------------------------------
-- EvaluateTx Parser
-------------------------------------------------
pEvaluateTx :: Parser Command
pEvaluateTx = 
  EvaluateTx 
    <$> pNetwork
    <*> pEndpoint
    <*> pTxFile

-------------------------------------------------
-- Query Parser
-------------------------------------------------
parseQuery :: Parser Command
parseQuery = fmap Query . hsubparser $ mconcat
  [ command "own-swaps" $
      info parseQueryOwnSwaps $ progDesc "Query your own swaps." 
  , command "all-swaps" $
      info parseQueryAll $ progDesc "Query all swaps." 
  , command "personal-address" $
      info pQueryPersonal $ progDesc "Query your personal address." 
  , command "protocol-params" $
      info pQueryParams $ progDesc "Query the current protocol parameters."
  , command "current-slot" $
      info pQueryCurrentSlot $ progDesc "Query the current slot number."
  ]

pQueryCurrentSlot :: Parser Query
pQueryCurrentSlot =
  QueryCurrentSlot
    <$> pNetwork
    <*> pEndpoint

pQueryPersonal :: Parser Query
pQueryPersonal =
  QueryPersonal
    <$> pNetwork
    <*> pEndpoint
    <*> pUserAddress
    <*> pFormat
    <*> pOutput

pQueryParams :: Parser Query
pQueryParams =
  QueryParameters
    <$> pNetwork
    <*> pOutput

parseQueryOwnSwaps :: Parser Query
parseQueryOwnSwaps = hsubparser $ mconcat
  [ command "one-way" $
      info parseQueryOwnOneWaySwaps $ progDesc "Query your own one-way swaps." 
  , command "two-way" $
      info parseQueryOwnTwoWaySwaps $ progDesc "Query your own two-way swaps." 
  ]

parseQueryOwnOneWaySwaps :: Parser Query
parseQueryOwnOneWaySwaps = fmap QueryOwnSwaps . hsubparser $ mconcat
  [ command "all"
      (info pQueryAll $ progDesc "Query all of your own swaps.")
  , command "offer"
      (info pQueryOffer $ progDesc "Query swaps by offer asset.")
  , command "ask"
      (info pQueryAsk $ progDesc "Query swaps by ask asset.")
  , command "trading-pair"
      (info pQueryTradingPair $ progDesc "Query swaps by trading pair.")
  ]
  where
    pQueryAll :: Parser QueryOwnSwaps
    pQueryAll =
      QueryOwnOneWaySwaps
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> pFormat
        <*> pOutput

    pQueryOffer :: Parser QueryOwnSwaps
    pQueryOffer =
      QueryOwnOneWaySwapsByOffer
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> (OfferAsset <$> pAssetConfig "offer")
        <*> pFormat
        <*> pOutput

    pQueryAsk :: Parser QueryOwnSwaps
    pQueryAsk =
      QueryOwnOneWaySwapsByAsk
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> (AskAsset <$> pAssetConfig "ask")
        <*> pFormat
        <*> pOutput


    pQueryTradingPair :: Parser QueryOwnSwaps
    pQueryTradingPair =
      QueryOwnOneWaySwapsByTradingPair
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> (OfferAsset <$> pAssetConfig "offer")
        <*> (AskAsset <$> pAssetConfig "ask")
        <*> pFormat
        <*> pOutput

parseQueryOwnTwoWaySwaps :: Parser Query
parseQueryOwnTwoWaySwaps = fmap QueryOwnSwaps . hsubparser $ mconcat
  [ command "all"
      (info pQueryAll $ progDesc "Query all of your own swaps.")
  , command "offer"
      (info pQueryOffer $ progDesc "Query swaps by offer asset.")
  , command "ask"
      (info pQueryAsk $ progDesc "Query swaps by ask asset.")
  , command "trading-pair"
      (info pQueryTradingPair $ progDesc "Query swaps by trading pair.")
  ]
  where
    pQueryAll :: Parser QueryOwnSwaps
    pQueryAll =
      QueryOwnTwoWaySwaps
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> pFormat
        <*> pOutput

    pQueryOffer :: Parser QueryOwnSwaps
    pQueryOffer =
      QueryOwnTwoWaySwapsByOffer
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> pAssetConfig "offer"
        <*> pFormat
        <*> pOutput

    pQueryAsk :: Parser QueryOwnSwaps
    pQueryAsk =
      QueryOwnTwoWaySwapsByAsk
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> pAssetConfig "ask" 
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryOwnSwaps
    pQueryTradingPair =
      QueryOwnTwoWaySwapsByTradingPair
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> ((,) <$> pAssetConfig "first" <*> pAssetConfig "second")
        <*> pFormat
        <*> pOutput

parseQueryAll :: Parser Query
parseQueryAll = fmap QueryAllSwaps . hsubparser $ mconcat
  [ command "offer"
      (info pQueryOffer $ progDesc "Query swaps by offer asset.")
  , command "ask"
      (info pQueryAsk $ progDesc "Query swaps by ask asset.")
  , command "trading-pair"
      (info pQueryTradingPair $ progDesc "Query swaps by trading pair.")
  ]
  where
    pQueryOffer :: Parser QueryAll
    pQueryOffer =
      QueryAllSwapsByOffer
        <$> pNetwork
        <*> pEndpoint
        <*> (OfferAsset <$> pAssetConfig "offer")
        <*> pFormat
        <*> pOutput

    pQueryAsk :: Parser QueryAll
    pQueryAsk =
      QueryAllSwapsByAsk
        <$> pNetwork
        <*> pEndpoint
        <*> (AskAsset <$> pAssetConfig "ask")
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryAll
    pQueryTradingPair =
      QueryAllSwapsByTradingPair
        <$> pNetwork
        <*> pEndpoint
        <*> (OfferAsset <$> pAssetConfig "offer")
        <*> (AskAsset <$> pAssetConfig "ask")
        <*> pFormat
        <*> pOutput

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "Save to file."
  <> completer (bashCompleter "file")
  )

pPrice :: String -> Parser PlutusRational
pPrice prefix = option (eitherReader readPlutusRational)
  (  long (prefix <> "-price")
  <> metavar "PRICE"
  <> help ("The price to take the " <> prefix <> " asset (fraction or decimal).")
  )

pPrevInput :: Parser (Maybe TxOutRef)
pPrevInput = pTxOutRef <|> pure Nothing
  where
    pTxOutRef :: Parser (Maybe TxOutRef)
    pTxOutRef = Just <$> option (eitherReader readTxOutRef)
      (  long "input-swap-ref"
      <> metavar "STRING"
      <> help "The output reference for the corresponding swap input (tx_hash#index)."
      )

pAssetConfig :: String -> Parser AssetConfig
pAssetConfig prefix = option (eitherReader readAssetConfig)
  (  long (prefix <> "-asset")
  <> metavar "STRING"
  <> help ("The " <> prefix <> " asset (lovelace or policy_id.asset_name).")
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet <|> pMainnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = flag' PreProdTestnet
      (  long "testnet"
      <> help "For preproduction testnet.")

    pMainnet :: Parser Network
    pMainnet = flag' Mainnet
      (  long "mainnet"
      <> help "For mainnet.")

pEndpoint :: Parser Endpoint
pEndpoint = pure Koios
  -- where
  --   pKoios :: Parser Endpoint
  --   pKoios = flag' Koios
  --     (  long "koios"
  --     <> help "Use Koios."
  --     )

pFormat :: Parser Format
pFormat = pJSON <|> pPretty <|> pPlain
  where
    pJSON :: Parser Format
    pJSON = flag' JSON
      (  long "json"
      <> help "Format as JSON."
      )

    pPretty :: Parser Format
    pPretty = flag' Pretty
      (  long "pretty"
      <> help "Format for pretty-printing."
      )

    pPlain :: Parser Format
    pPlain = flag' Plain
      (  long "plain"
      <> help "Format for pretty-printing without colors."
      )

pUserAddress :: Parser UserAddress
pUserAddress = UserAddress <$> strOption
  (  long "address"
  <> metavar "STRING"
  <> help "Address in bech32 format."
  )

pTxFile :: Parser FilePath
pTxFile = strOption
  (  long "tx-file"
  <> metavar "STRING"
  <> help "Transaction file path."
  )

pSwapExpiration :: Parser (Maybe POSIXTime)
pSwapExpiration = pExpr <|> pure Nothing
  where
    pExpr :: Parser (Maybe POSIXTime)
    pExpr = Just . POSIXTime <$> option auto
      (  long "expiration"
      <> metavar "TIME"
      <> help "The expiration time for the order in POSIX time (milliseconds)."
      )
