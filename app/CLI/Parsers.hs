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
  [ command "export-script"
      (info parseExportScript $ progDesc "Export a dApp plutus script.")
  , command "swap-datum"
      (info pCreateDatum $ progDesc "Create a swap datum for the dApp.")
  , command "swap-redeemer"
      (info  pCreateSwapRedeemer $ progDesc "Create a redeemer for the universal swap script.")
  , command "beacon-redeemer"
      (info parseCreateBeaconRedeemer $ progDesc "Create a redeemer for the beacon policy.")
  , command "beacon-info"
      (info parseBeaconInfo $ progDesc "Generate a beacon policy id or asset name.")
  , command "query"
      (info parseQuery $ progDesc "Query the blockchain.")
  , command "submit"
      (info pSubmit $ progDesc "Submit a transaction to the blockchain.")
  , command "export-protocol-params"
      (info pExportParams $ progDesc "Export the current protocol parameters.")
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = hsubparser $ mconcat
    [ command "beacon-policy"
        (info pExportPolicy $ progDesc "Export the beacon policy for a specific offer asset.")
    , command "swap-script"
        (info pExportSwap $ progDesc "Export the universal swap validator script.")
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = 
      ExportScript 
        <$> (BeaconPolicy . OfferAsset <$> pOfferAsset)
        <*> pOutputFile
    
    pExportSwap :: Parser Command
    pExportSwap = 
      ExportScript 
        <$> pure SwapScript
        <*> pOutputFile

-------------------------------------------------
-- CreateDatum Parser
-------------------------------------------------
pCreateDatum :: Parser Command
pCreateDatum = CreateDatum <$> pInternalSwapDatum <*> pOutputFile
  where
    pInternalSwapDatum :: Parser InternalSwapDatum
    pInternalSwapDatum = 
      InternalSwapDatum 
        <$> (OfferAsset <$> pOfferAsset)
        <*> (AskAsset <$> pAskAsset)
        <*> pSwapPrice

-------------------------------------------------
-- Swap Redeemer Parser
-------------------------------------------------
pCreateSwapRedeemer :: Parser Command
pCreateSwapRedeemer = CreateSwapRedeemer <$> (pCloseOrUpdate <|> pSwap) <*> pOutputFile
  where
    pCloseOrUpdate :: Parser SwapRedeemer
    pCloseOrUpdate = flag' CloseOrUpdate
      (  long "close-or-update"
      <> help "Close or update swap positions."
      )

    pSwap :: Parser SwapRedeemer
    pSwap = flag' Swap
      (  long "swap" 
      <> help "Swap with assets at a swap address."
      )

-------------------------------------------------
-- Beacon Redeemer Parser
-------------------------------------------------
parseCreateBeaconRedeemer :: Parser Command
parseCreateBeaconRedeemer = hsubparser $ mconcat
    [ command "mint"
        (info pMint $ progDesc "Create a beacon minting redeemer.")
    , command "burn"
        (info pBurn $ progDesc "Create a beacon burning redeemer.")
    ]
  where
    pMint :: Parser Command
    pMint = CreateBeaconRedeemer <$> (CreateSwap <$> some pAskAsset) <*> pOutputFile

    pBurn :: Parser Command
    pBurn = CreateBeaconRedeemer <$> pure BurnBeacons <*> pOutputFile

-------------------------------------------------
-- Beacon Info Parser
-------------------------------------------------
parseBeaconInfo :: Parser Command
parseBeaconInfo = hsubparser $ mconcat
    [ command "policy-id"
        (info pGenPolicyId $ progDesc "Generate the beacon policy id for a specific offer asset.")
    , command "asset-name"
        (info pGenAssetName $ progDesc "Generate the beacon asset name for a specific ask asset.")
    , command "full-name"
        (info pGenFullName $ progDesc "Generate the full beacon name for a specific trading pair.")
    ]
  where
    pGenPolicyId :: Parser Command
    pGenPolicyId = 
      BeaconInfo 
        <$> (PolicyId . OfferAsset <$> pOfferAsset)
        <*> pOutput

    pGenAssetName :: Parser Command
    pGenAssetName =
      BeaconInfo
        <$> (AssetName . AskAsset <$> pAskAsset)
        <*> pOutput

    pGenFullName :: Parser Command
    pGenFullName =
      BeaconInfo
        <$> ( FullName 
                <$> (OfferAsset <$> pOfferAsset) 
                <*> (AskAsset <$> pAskAsset)
            )
        <*> pOutput

-------------------------------------------------
-- QueryBeacons Parser
-------------------------------------------------
parseQuery :: Parser Command
parseQuery = fmap Query . hsubparser $ mconcat
  [ command "own-swaps"
      (info parseQueryOwn $ progDesc "Query your own swaps.") 
  , command "all-swaps"
      (info parseQueryAll $ progDesc "Query all swaps.") 
  , command "personal-address"
      (info pQueryPersonal $ progDesc "Query your personal address.") 
  ]

parseQueryOwn :: Parser Query
parseQueryOwn = fmap QueryOwn . hsubparser $ mconcat
  [ command "all"
      (info pQueryAll $ progDesc "Query all of your own swaps.")
  , command "offer"
      (info pQueryOffer $ progDesc "Query swaps by offer asset.")
  , command "trading-pair"
      (info pQueryTradingPair $ progDesc "Query swaps by trading pair.")
  ]
  where
    pQueryAll :: Parser QueryOwn
    pQueryAll =
      QueryOwnSwaps
        <$> pNetwork
        <*> pQueryEndpoint
        <*> pUserAddress
        <*> pFormat
        <*> pOutput

    pQueryOffer :: Parser QueryOwn
    pQueryOffer =
      QueryOwnSwapsByOffer
        <$> pNetwork
        <*> pQueryEndpoint
        <*> pUserAddress
        <*> (OfferAsset <$> pOfferAsset)
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryOwn
    pQueryTradingPair =
      QueryOwnSwapsByTradingPair
        <$> pNetwork
        <*> pQueryEndpoint
        <*> pUserAddress
        <*> (OfferAsset <$> pOfferAsset)
        <*> (AskAsset <$> pAskAsset)
        <*> pFormat
        <*> pOutput

parseQueryAll :: Parser Query
parseQueryAll = fmap QueryAll . hsubparser $ mconcat
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
        <*> pQueryEndpoint
        <*> (OfferAsset <$> pOfferAsset)
        <*> pFormat
        <*> pOutput

    pQueryTradingPair :: Parser QueryAll
    pQueryTradingPair =
      QueryAllSwapsByTradingPair
        <$> pNetwork
        <*> pQueryEndpoint
        <*> (OfferAsset <$> pOfferAsset)
        <*> (AskAsset <$> pAskAsset)
        <*> pFormat
        <*> pOutput

pQueryPersonal :: Parser Query
pQueryPersonal =
  QueryPersonal
    <$> pNetwork
    <*> pQueryEndpoint
    <*> pUserAddress
    <*> pFormat
    <*> pOutput

-------------------------------------------------
-- ExportParams Parser
-------------------------------------------------
pExportParams :: Parser Command
pExportParams =
  ExportParams
    <$> pNetwork
    <*> pOutput

-------------------------------------------------
-- Submit Parser
-------------------------------------------------
pSubmit :: Parser Command
pSubmit = 
    Submit 
      <$> pNetwork
      <*> pQueryEndpoint
      <*> pTxFile
  where
    pTxFile :: Parser FilePath
    pTxFile = strOption
      (  long "tx-file"
      <> metavar "STRING"
      <> help "Transaction file path."
      )

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

pOfferAsset :: Parser AssetConfig
pOfferAsset = pOfferLovelace <|> ((,) <$> pOfferSymbol <*> pOfferName)
  where
    pOfferLovelace :: Parser AssetConfig
    pOfferLovelace = flag' (adaSymbol,adaToken)
      (  long "offer-lovelace"
      <> help "The offered asset is lovelace."
      )

    pOfferSymbol :: Parser CurrencySymbol
    pOfferSymbol = option (eitherReader readCurrencySymbol)
      (  long "offer-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the offered asset."
      )
    
    pOfferName :: Parser TokenName
    pOfferName = option (eitherReader readTokenName)
      (  long "offer-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the offered asset."
      )

pAskAsset :: Parser AssetConfig
pAskAsset = pAskLovelace <|> ((,) <$> pAskSymbol <*> pAskName)
  where
    pAskLovelace :: Parser AssetConfig
    pAskLovelace = flag' (adaSymbol,adaToken)
      (  long "ask-lovelace"
      <> help "The asked asset is lovelace."
      )

    pAskSymbol :: Parser CurrencySymbol
    pAskSymbol = option (eitherReader readCurrencySymbol)
      (  long "ask-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the asked asset."
      )
    
    pAskName :: Parser TokenName
    pAskName = option (eitherReader readTokenName)
      (  long "ask-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the asked asset."
      )

pSwapPrice :: Parser PlutusRational
pSwapPrice = pPrice <|> pWeightedPrice
  where
    pPrice :: Parser PlutusRational
    pPrice = unsafeRatio <$> pPriceNum <*> pPriceDen

    pPriceNum :: Parser Integer
    pPriceNum = option auto
      ( long "price-numerator"
      <> metavar "INT"
      <> help "The numerator of the swap price."
      )

    pPriceDen :: Parser Integer
    pPriceDen = option auto
      ( long "price-denominator"
      <> metavar "INT"
      <> help "The denominator of the swap price."
      )

    pWeightedPrice :: Parser PlutusRational
    pWeightedPrice = calcWeightedPrice <$> some pUtxoPriceInfo

    pUtxoPriceInfo :: Parser UtxoPriceInfo
    pUtxoPriceInfo = UtxoPriceInfo
      <$> pAmount
      <*> pPrice
    
    pAmount :: Parser Integer
    pAmount = option auto
      (  long "utxo-balance"
      <> metavar "INT"
      <> help "How much of the target asset is in this UTxO."
      )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
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

pQueryEndpoint :: Parser QueryEndpoint
pQueryEndpoint = pure Koios
  -- where
  --   pKoios :: Parser ApiEndpoint
  --   pKoios = flag' Koios
  --     (  long "koios"
  --     <> help "Query using Koios."
  --     )

pUserAddress :: Parser UserAddress
pUserAddress = UserAddress <$> strOption
  (  long "address"
  <> metavar "STRING"
  <> help "Address in bech32 format."
  )
