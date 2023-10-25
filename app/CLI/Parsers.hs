module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative

import CardanoSwaps
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "scripts" $
      info parseExportScript $ progDesc "Export a dApp plutus script."
  , command "datums" $
      info parseCreateDatum $ progDesc "Create a datum for the dApp."
  , command "spending-redeemers" $
      info  parseCreateSpendingRedeemer $ progDesc "Create a spending redeemer."
  , command "beacon-redeemers" $
      info parseCreateMintingRedeemer $ progDesc "Create a redeemer for the beacon policy."
  , command "beacon-info" $
      info parseBeaconInfo $ progDesc "Calculate a beacon policy id or asset name."
  , command "query" $
      info parseQuery $ progDesc "Query the blockchain."
  , command "submit" $
      info pSubmit $ progDesc "Submit a transaction to the blockchain."
  , command "protocol-params" $
      info pExportParams $ progDesc "Export the current protocol parameters."
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
    [ command "beacon-policy" $
        info pExportPolicy $ progDesc "Export the beacon policy for one-way swaps."
    , command "swap-script" $
        info pExportSwap $ progDesc "Export the swap validator for one-way swaps."
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = 
      ExportScript 
        <$> pure OneWayBeaconPolicy
        <*> pOutputFile
    
    pExportSwap :: Parser Command
    pExportSwap = 
      ExportScript 
        <$> pure OneWaySwapScript
        <*> pOutputFile

parseTwoWayExportScript :: Parser Command
parseTwoWayExportScript = hsubparser $ mconcat
    [ command "beacon-policy" $
        info pExportPolicy $ progDesc "Export the beacon policy for two-way swaps."
    , command "swap-script" $
        info pExportSwap $ progDesc "Export the swap validator for two-way swaps."
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = 
      ExportScript 
        <$> pure TwoWayBeaconPolicy
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
        <$> (OfferAsset <$> pOneWayAsset "offer")
        <*> (AskAsset <$> pOneWayAsset "ask")
        <*> pPrice "price"
        <*> pPrevInput

pCreateTwoWayDatum :: Parser Command
pCreateTwoWayDatum = CreateDatum <$> pInternalTwoWaySwapDatum <*> pOutputFile
  where
    pInternalTwoWaySwapDatum :: Parser InternalDatum
    pInternalTwoWaySwapDatum = 
      InternalTwoWaySwapDatum 
        <$> (fmap TwoWayPair . (,) <$> pTwoWayAsset "first" <*> pTwoWayAsset "second")
        <*> pPrice "forward-price"
        <*> pPrice "reverse-price"
        <*> pPrevInput

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
      <$> (OneWaySpendingRedeemer <$> (pCloseOrUpdate <|> pSwap))
      <*> pOutputFile
  where
    pCloseOrUpdate :: Parser OneWaySwapRedeemer
    pCloseOrUpdate = flag' OneWayCloseOrUpdate
      (  long "close-or-update"
      <> help "Close or update swap positions."
      )

    pSwap :: Parser OneWaySwapRedeemer
    pSwap = flag' OneWaySwap
      (  long "swap" 
      <> help "Swap with assets at a swap address."
      )

pCreateTwoWaySpendingRedeemer :: Parser Command
pCreateTwoWaySpendingRedeemer = 
    CreateSpendingRedeemer 
      <$> (TwoWaySpendingRedeemer <$> (pCloseOrUpdate <|> pForwardSwap <|> pReverseSwap))
      <*> pOutputFile
  where
    pCloseOrUpdate :: Parser TwoWaySwapRedeemer
    pCloseOrUpdate = flag' TwoWayCloseOrUpdate
      (  long "close-or-update"
      <> help "Close or update swap positions."
      )

    pForwardSwap :: Parser TwoWaySwapRedeemer
    pForwardSwap = flag' TwoWayForwardSwap
      (  long "forward-swap" 
      <> help "Take asset2 from a swap."
      )

    pReverseSwap :: Parser TwoWaySwapRedeemer
    pReverseSwap = flag' TwoWayReverseSwap
      (  long "reverse-swap" 
      <> help "Take asset1 from a swap."
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
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser MintingRedeemer
    pMint = flag' (OneWayMintingRedeemer OneWayCreateSwap)
      (  long "create-swap"
      <> help "Mint the beacons for a new swap UTxO."
      )

    pBurn :: Parser MintingRedeemer
    pBurn = flag' (OneWayMintingRedeemer OneWayCreateSwap)
      (  long "burn"
      <> help "Burn the beacons for a swap UTxO."
      )

pCreateTwoWayBeaconRedeemer :: Parser Command
pCreateTwoWayBeaconRedeemer = 
    CreateMintingRedeemer
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser MintingRedeemer
    pMint = flag' (TwoWayMintingRedeemer TwoWayCreateSwap)
      (  long "create-swap"
      <> help "Mint the beacons for a new swap UTxO."
      )

    pBurn :: Parser MintingRedeemer
    pBurn = flag' (TwoWayMintingRedeemer TwoWayCreateSwap)
      (  long "burn"
      <> help "Burn the beacons for a swap UTxO."
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
        info pOfferName $ progDesc "Calculate the one-way offer beacon asset name."
    , command "pair-beacon" $
        info pPairName $ progDesc "Calculate the one-way trading pair beacon asset name."
    ]
  where
    pPolicyId :: Parser Command
    pPolicyId = BeaconInfo <$> pure OneWayPolicyId <*> pOutput

    pOfferName :: Parser Command
    pOfferName = 
      BeaconInfo
        <$> (OneWayOfferBeaconAssetName <$> (OfferAsset <$> pOneWayAsset "offer"))
        <*> pOutput

    pPairName :: Parser Command
    pPairName = 
      BeaconInfo
        <$> ( fmap OneWayPairBeaconAssetName . (,) 
                <$> (OfferAsset <$> pOneWayAsset "offer") <*> (AskAsset <$> pOneWayAsset "ask")
            )
        <*> pOutput

pTwoWayBeaconInfo :: Parser Command
pTwoWayBeaconInfo = hsubparser $ mconcat
    [ command "policy-id" $
        info pPolicyId $ progDesc "Calculate the two-way beacon policy id."
    , command "offer-beacon" $
        info pOfferName $ progDesc "Calculate the two-way offer beacon asset name."
    , command "pair-beacon" $
        info pPairName $ progDesc "Calculate the two-way trading pair beacon asset name."
    ]
  where
    pPolicyId :: Parser Command
    pPolicyId = BeaconInfo <$> pure TwoWayPolicyId <*> pOutput

    pOfferName :: Parser Command
    pOfferName = 
      BeaconInfo
        <$> ( TwoWayOfferBeaconAssetName 
                <$> (pTwoWayAsset "first" <|> pTwoWayAsset "second")
            )
        <*> pOutput

    pPairName :: Parser Command
    pPairName = 
      BeaconInfo
        <$> ( fmap TwoWayPairBeaconAssetName . fmap TwoWayPair . (,) 
                <$> (pTwoWayAsset "first") <*> (pTwoWayAsset "second")
            )
        <*> pOutput

-------------------------------------------------
-- Submit Parser
-------------------------------------------------
pSubmit :: Parser Command
pSubmit = 
    Submit 
      <$> pNetwork
      <*> pEndpoint
      <*> pTxFile
  where
    pTxFile :: Parser FilePath
    pTxFile = strOption
      (  long "tx-file"
      <> metavar "STRING"
      <> help "Transaction file path."
      )

-------------------------------------------------
-- ExportParams Parser
-------------------------------------------------
pExportParams :: Parser Command
pExportParams =
  ExportParams
    <$> pNetwork
    <*> pOutput

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
  ]

pQueryPersonal :: Parser Query
pQueryPersonal =
  QueryPersonal
    <$> pNetwork
    <*> pEndpoint
    <*> pUserAddress
    <*> pFormat
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
        <*> (OfferAsset <$> pOneWayAsset "offer")
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryOwnSwaps
    pQueryTradingPair =
      QueryOwnOneWaySwapsByTradingPair
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> (OfferAsset <$> pOneWayAsset "offer")
        <*> (AskAsset <$> pOneWayAsset "ask")
        <*> pFormat
        <*> pOutput

parseQueryOwnTwoWaySwaps :: Parser Query
parseQueryOwnTwoWaySwaps = fmap QueryOwnSwaps . hsubparser $ mconcat
  [ command "all"
      (info pQueryAll $ progDesc "Query all of your own swaps.")
  , command "offer"
      (info pQueryOffer $ progDesc "Query swaps by offer asset.")
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
        <*> (pTwoWayAsset "first" <|> pTwoWayAsset "second")
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryOwnSwaps
    pQueryTradingPair =
      QueryOwnTwoWaySwapsByTradingPair
        <$> pNetwork
        <*> pEndpoint
        <*> pUserAddress
        <*> (fmap TwoWayPair . (,) <$> pTwoWayAsset "first" <*> pTwoWayAsset "second")
        <*> pFormat
        <*> pOutput

parseQueryAll :: Parser Query
parseQueryAll = fmap QueryAllSwaps . hsubparser $ mconcat
  [ command "offer"
      (info pQueryOffer $ progDesc "Query swaps by offer asset.")
  , command "trading-pair"
      (info pQueryTradingPair $ progDesc "Query swaps by trading pair.")
  ]
  where
    pQueryOffer :: Parser QueryAll
    pQueryOffer =
      QueryAllSwapsByOffer
        <$> pNetwork
        <*> pEndpoint
        <*> (OfferAsset <$> pOneWayAsset "offer")
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryAll
    pQueryTradingPair =
      QueryAllSwapsByTradingPair
        <$> pNetwork
        <*> pEndpoint
        <*> (OfferAsset <$> pOneWayAsset "offer")
        <*> (AskAsset <$> pOneWayAsset "ask")
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

pOneWayAsset :: String -> Parser AssetConfig
pOneWayAsset asset = pLovelace <|> ((,) <$> pSymbol <*> pName)
  where
    pLovelace :: Parser AssetConfig
    pLovelace = flag' (adaSymbol,adaToken)
      (  long (asset <> "-lovelace")
      <> help ("The " <> asset <> " asset is lovelace.")
      )

    pSymbol :: Parser CurrencySymbol
    pSymbol = option (eitherReader readCurrencySymbol)
      (  long (asset <> "-policy-id") 
      <> metavar "STRING" 
      <> help ("The policy id of the " <> asset <> " asset.")
      )
    
    pName :: Parser TokenName
    pName = option (eitherReader readTokenName)
      (  long (asset <> "-token-name")
      <> metavar "STRING"
      <> help ("The token name (in hexidecimal) of the " <> asset <> " asset.")
      )

pPrice :: String -> Parser PlutusRational
pPrice prefix = unsafeRatio <$> pPriceNum <*> pPriceDen
  where
    pPriceNum :: Parser Integer
    pPriceNum = option auto
      ( long (prefix <> "-numerator")
      <> metavar "INT"
      <> help ("The numerator of the " <> prefix <> ".")
      )

    pPriceDen :: Parser Integer
    pPriceDen = option auto
      ( long (prefix <> "-denominator")
      <> metavar "INT"
      <> help ("The denominator of the " <> prefix <> ".")
      )

pPrevInput :: Parser (Maybe TxOutRef)
pPrevInput = pTxOutRef <|> pure Nothing 
  where
    pTxOutRef :: Parser (Maybe TxOutRef)
    pTxOutRef = Just <$> (TxOutRef <$> pTxId <*> pOutputIndex)

    pTxId :: Parser TxId
    pTxId = option (eitherReader readTxId)
      (  long "tx-hash"
      <> metavar "STRING"
      <> help "The transaction hash for the corresponding swap input."
      )

    pOutputIndex :: Parser Integer
    pOutputIndex = option auto
      (  long "output-index"
      <> metavar "STRING"
      <> help "The output index for the corresponding swap input."
      )

pTwoWayAsset :: String -> Parser AssetConfig
pTwoWayAsset prefix = pLovelace <|> ((,) <$> pSymbol <*> pName)
  where
    pLovelace :: Parser AssetConfig
    pLovelace = flag' (adaSymbol,adaToken)
      (  long (prefix <> "-asset-lovelace")
      <> help ("The " <> prefix <> " asset is lovelace.")
      )

    pSymbol :: Parser CurrencySymbol
    pSymbol = option (eitherReader readCurrencySymbol)
      (  long (prefix <> "-policy-id") 
      <> metavar "STRING" 
      <> help ("The policy id of the " <> prefix <> "asset.")
      )
    
    pName :: Parser TokenName
    pName = option (eitherReader readTokenName)
      (  long (prefix <> "-token-name")
      <> metavar "STRING"
      <> help ("The token name (in hexidecimal) of " <> prefix <> "asset.")
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
      <> help "Query the preproduction testnet.")

    pMainnet :: Parser Network
    pMainnet = flag' Mainnet
      (  long "mainnet"
      <> help "Query the mainnet.")

pEndpoint :: Parser Endpoint
pEndpoint = pKoios 
  where
    pKoios :: Parser Endpoint
    pKoios = flag' Koios
      (  long "koios"
      <> help "Use Koios."
      )

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
