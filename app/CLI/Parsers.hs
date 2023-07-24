{-# LANGUAGE OverloadedStrings #-}

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
  , command "generate-beacon-name"
      (info pGenerateBeaconFullName $ progDesc "Generate the beacon name for a specific trading pair.")
  , command "query"
      (info parseQueryBeacons $ progDesc "Query the dApp's beacon tokens.")
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
        <$> (BeaconPolicy <$> pOfferConfig)
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
pCreateDatum = CreateDatum <$> pSwapDatumInfo <*> pOutputFile
  where
    pSwapDatumInfo :: Parser SwapDatumInfo
    pSwapDatumInfo = 
      SwapDatumInfo 
        <$> pOfferConfig 
        <*> pAskConfig 
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
    pMint = CreateBeaconRedeemer <$> (MintBeacons <$> some pAskConfig) <*> pOutputFile

    pBurn :: Parser Command
    pBurn = CreateBeaconRedeemer <$> pure BurnBeacons <*> pOutputFile

-------------------------------------------------
-- Generate Beacon Full Name Parser
-------------------------------------------------
pGenerateBeaconFullName :: Parser Command
pGenerateBeaconFullName =
  GenerateBeaconFullName
    <$> pOfferConfig
    <*> pAskConfig
    <*> pOutput

-------------------------------------------------
-- QueryBeacons Parser
-------------------------------------------------
parseQueryBeacons :: Parser Command
parseQueryBeacons = fmap QueryBeacons . hsubparser $ mconcat
    [ command "all-swaps-by-pair"
        (info pQueryAllBeaconsByTradingPair $ progDesc "Query available swaps for a specific trading pair.")
    , command "all-swaps-by-offer"
        (info pQueryAllSwapsByOffer $ progDesc "Query all swaps with a specific offer asset.")
    , command "all-own-swaps"
        (info pQueryOwnSwaps $ progDesc "Query all own swaps")
    , command "all-own-swaps-by-offer"
        (info pQueryOwnSwapsByOffer $ progDesc "Query all own swaps with a specific offer asset.")
    , command "all-own-swaps-by-pair"
        (info pQueryOwnSwapsByTradingPair $ progDesc "Query all own swaps for a specific trading pair.")
    ]
  where
    pQueryAllBeaconsByTradingPair :: Parser Query
    pQueryAllBeaconsByTradingPair =
      QueryAllSwapsByTradingPair
        <$> pNetwork
        <*> pApiEndpoint
        <*> pOfferConfig
        <*> pAskConfig
        <*> pOutput

    pQueryAllSwapsByOffer :: Parser Query
    pQueryAllSwapsByOffer =
      QueryAllSwapsByOffer
        <$> pOfferConfig
        <*> pOutput

    pQueryOwnSwaps :: Parser Query
    pQueryOwnSwaps =
      QueryOwnSwaps
        <$> pNetwork
        <*> pApiEndpoint
        <*> pSwapAddress
        <*> pOutput

    pQueryOwnSwapsByOffer :: Parser Query
    pQueryOwnSwapsByOffer =
      QueryOwnSwapsByOffer
        <$> pSwapAddress
        <*> pOfferConfig
        <*> pOutput

    pQueryOwnSwapsByTradingPair :: Parser Query
    pQueryOwnSwapsByTradingPair =
      QueryOwnSwapsByTradingPair
        <$> pNetwork
        <*> pApiEndpoint
        <*> pSwapAddress
        <*> pOfferConfig
        <*> pAskConfig
        <*> pOutput
    

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )

pOfferConfig :: Parser AssetConfig
pOfferConfig = pOfferLovelace <|> (AssetConfig <$> pOfferSymbol <*> pOfferName)
  where
    pOfferLovelace :: Parser AssetConfig
    pOfferLovelace = flag' (AssetConfig adaSymbol adaToken)
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

pAskConfig :: Parser AssetConfig
pAskConfig = pAskLovelace <|> (AssetConfig <$> pAskSymbol <*> pAskName)
  where
    pAskLovelace :: Parser AssetConfig
    pAskLovelace = flag' (AssetConfig adaSymbol adaToken)
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

pNetwork :: Parser Network
pNetwork = pPreProdTestnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = flag' PreProdTestnet
      (  long "testnet"
      <> help "Query the preproduction testnet.")

pApiEndpoint :: Parser ApiEndpoint
pApiEndpoint = pKoios <|> pBlockfrost
  where
    pKoios :: Parser ApiEndpoint
    pKoios = flag' Koios
      (  long "koios"
      <> help "Query using Koios."
      )

    pBlockfrost :: Parser ApiEndpoint
    pBlockfrost = Blockfrost <$> strOption
      (  long "blockfrost"
      <> metavar "STRING"
      <> help "Query using Blockfrost with the supplied api key."
      )

pSwapAddress :: Parser SwapAddress
pSwapAddress = SwapAddress <$> strOption
  (  long "address"
  <> metavar "STRING"
  <> help "Address in bech32 format."
  )