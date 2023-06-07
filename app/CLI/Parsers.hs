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
  , command "datum"
      (info parseCreateSwapDatum $ progDesc "Create a datum for the dApp.")
  , command "swap-redeemer"
      (info  pCreateSwapRedeemer $ progDesc "Create a redeemer for the swap validator.")
  , command "beacon-redeemer"
      (info pCreateBeaconRedeemer $ progDesc "Create a redeemer for the beacon policy.")
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = hsubparser $ mconcat
    [ command "beacon-policy"
        (info pExportPolicy $ progDesc "Export the beacon policy for a specific pair.")
    , command "swap-script"
        (info pExportSwap $ progDesc "Export the swap validator script for a specific pair.")
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = 
      ExportScript 
        <$> (BeaconPolicy <$> pSwapConfig)
        <*> pOutputFile
    
    pExportSwap :: Parser Command
    pExportSwap = 
      ExportScript 
        <$> (SwapScript <$> pSwapConfig)
        <*> pOutputFile

-------------------------------------------------
-- Datum Parser
-------------------------------------------------
parseCreateSwapDatum :: Parser Command
parseCreateSwapDatum = hsubparser $ mconcat
    [ command "beacon-datum"
        (info pBeaconDatum $ progDesc "Create the datum for a Beacon UTxO")
    , command "swap-datum"
        (info pSwapDatum $ progDesc "Create the datum for a swappable UTxO")
    ]
  where
    pSwapDatum :: Parser Command
    pSwapDatum = CreateSwapDatum <$> pDatumPrice <*> pOutputFile

    pBeaconDatum :: Parser Command
    pBeaconDatum = CreateSwapDatum <$> (SwapDatum . BeaconSymbol <$> pBeaconPolicy) <*> pOutputFile

-------------------------------------------------
-- Swap Redeemer Parser
-------------------------------------------------
pCreateSwapRedeemer :: Parser Command
pCreateSwapRedeemer = CreateSwapRedeemer <$> (pClose <|> pUpdate <|> pSwap) <*> pOutputFile
  where
    pUpdate :: Parser SwapRedeemer
    pUpdate = flag' Update
      (  long "update"
      <> help "Update swap positions."
      )

    pClose :: Parser SwapRedeemer
    pClose = flag' Close
      (  long "close"
      <> help "Burn beacon and reclaim deposit."
      )

    pSwap :: Parser SwapRedeemer
    pSwap = flag' Swap
      (  long "swap" 
      <> help "Swap with assets at a swap address."
      )

-------------------------------------------------
-- Beacon Redeemer Parser
-------------------------------------------------
pCreateBeaconRedeemer :: Parser Command
pCreateBeaconRedeemer =
    CreateBeaconRedeemer
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser BeaconRedeemer
    pMint = flag' MintBeacon
      (  long "mint-beacon"
      <> help "Mint a beacon for the dApp."
      )

    pBurn :: Parser BeaconRedeemer
    pBurn = flag' BurnBeacon
      (  long "burn-beacon"
      <> help "Burn a beacon for the dApp."
      )

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

pSwapConfig :: Parser SwapConfig
pSwapConfig = SwapConfig <$> pOfferedAsset <*> pAskedAsset

pOfferedAsset :: Parser (CurrencySymbol,TokenName)
pOfferedAsset = pOfferedAssetLovelace <|> ((,) <$> pOfferedAssetCurrencySymbol <*> pOfferedAssetTokenName)
  where
    pOfferedAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pOfferedAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "offered-asset-is-lovelace"
      <> help "The offered asset is lovelace"
      )

    pOfferedAssetCurrencySymbol :: Parser CurrencySymbol
    pOfferedAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "offered-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the offered asset."
      )

    pOfferedAssetTokenName :: Parser TokenName
    pOfferedAssetTokenName = option (eitherReader readTokenName)
      (  long "offered-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the offered asset."
      )

pAskedAsset :: Parser (CurrencySymbol,TokenName)
pAskedAsset = pAskedAssetLovelace <|> ((,) <$> pAskedAssetCurrencySymbol <*> pAskedAssetTokenName)
  where
    pAskedAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pAskedAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "asked-asset-is-lovelace"
      <> help "The asked asset is lovelace"
      )

    pAskedAssetCurrencySymbol :: Parser CurrencySymbol
    pAskedAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "asked-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the asked asset."
      )

    pAskedAssetTokenName :: Parser TokenName
    pAskedAssetTokenName = option (eitherReader readTokenName)
      (  long "asked-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the asked asset."
      )

pPrice :: Parser PlutusRational
pPrice = unsafeRatio <$> pPriceNum <*> pPriceDen
  where
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

pDatumPrice :: Parser Datum
pDatumPrice = pNewDatum <|> pWeightedPrice
  where
    pNewDatum :: Parser Datum
    pNewDatum = SwapDatum <$> (SwapPrice <$> pPrice)

    pWeightedPrice :: Parser Datum
    pWeightedPrice = WeightedPrice <$> some pUtxoPriceInfo

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

pBeaconPolicy :: Parser CurrencySymbol
pBeaconPolicy = option (eitherReader readCurrencySymbol)
  (  long "beacon-policy-id"
  <> metavar "STRING"
  <> help "Policy id for that trading pair's beacon policy.")