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
  [ command "swaps" 
      (info parseScriptCmd $ progDesc "Commands for swapping assets.")
  , command "beacons"
      (info parseBeaconsCmd $ progDesc "Commands for mintin/burning the DEX beacons.")
  , command "query"
      (info parseQueryAvailableSwaps $ progDesc "Command for querying available swaps for a trading pair.")
  ]

-------------------------------------------------
-- Query Parser
-------------------------------------------------
parseQueryAvailableSwaps :: Parser Command
parseQueryAvailableSwaps =
  QueryAvailableSwaps
    <$> pAsked
    <*> pOffered
    <*> pNetwork
    <*> pOutput

pNetwork :: Parser Network
pNetwork = pMainnet <|> pPreProdTestnet
  where
    pMainnet :: Parser Network
    pMainnet = Mainnet <$> strOption
      (  long "mainnet"
      <> metavar "STRING"
      <> help "Query the mainnet using the Blockfrost Api with the supplied api key.")
    
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = PreProdTestnet <$> strOption
      (  long "preprod-testnet"
      <> metavar "STRING"
      <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

-------------------------------------------------
-- Beacons Parser
-------------------------------------------------
parseBeaconsCmd :: Parser Command
parseBeaconsCmd = fmap BeaconCmd $ hsubparser $ mconcat
  [ command "export-policy"
      (info pExportBeaconPolicy $ progDesc "Export the beacon policy for a specific trading pair.")
  , command "create-datum"
      (info pCreateBeaconDatum $ progDesc "Create the special datum for storing the beacon in the swap address.")
  , command "create-redeemer"
      (info pCreateBeaconRedeemer $ progDesc "Create the redeemer for using the beacon policy.")
  ]

pExportBeaconPolicy :: Parser BeaconCmd
pExportBeaconPolicy = ExportBeaconPolicy <$> pAsked <*> pOffered <*> pOutputFile

pCreateBeaconDatum :: Parser BeaconCmd
pCreateBeaconDatum = CreateBeaconDatum <$> pAsked <*> pOffered <*> pDatumPrice <*> pOutputFile

pCreateBeaconRedeemer :: Parser BeaconCmd
pCreateBeaconRedeemer =
    CreateBeaconRedeemer
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser BeaconRedeemer
    pMint = flag' MintBeacon
      (  long "mint-beacon"
      <> help "Mint a beacon for the DEX."
      )

    pBurn :: Parser BeaconRedeemer
    pBurn = flag' BurnBeacon
      (  long "burn-beacon"
      <> help "Burn a beacon for the DEX."
      )

-------------------------------------------------
-- Swap-Scripts Parser
-------------------------------------------------
parseScriptCmd :: Parser Command
parseScriptCmd = fmap SwapCmd $ hsubparser $ mconcat
  [ command "export-script"
      (info pExportSwapScript $ progDesc "Export the swap script for a specific trading pair.")
  , command "create-datum"
      (info pCreateSwapDatum $ progDesc "Create the datum for basic swap outputs (not applicable for storing with beacons).")
  , command "create-redeemer"
      (info pCreateSwapRedeemer $ progDesc "Create the redeemer for the DEX.")
  ]

pExportSwapScript :: Parser SwapCmd
pExportSwapScript = ExportSwapScript <$> pAsked <*> pOffered <*> pOutputFile

pCreateSwapDatum :: Parser SwapCmd
pCreateSwapDatum = CreateSwapDatum <$> pDatumPrice <*> pOutputFile

pCreateSwapRedeemer :: Parser SwapCmd
pCreateSwapRedeemer = CreateSwapRedeemer <$> (pClose <|> pUpdate <|> pSwap) <*> pOutputFile
  where
    pUpdate :: Parser Action
    pUpdate = flag' Update
      (  long "update"
      <> help "Update swap positions."
      )

    pClose :: Parser Action
    pClose = flag' Close
      (  long "close"
      <> help "Burn beacon and reclaim deposit."
      )

    pSwap :: Parser Action
    pSwap = flag' Swap
      (  long "swap" 
      <> help "Swap with assets at a swap address."
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

pAsked :: Parser Asset
pAsked = pAskedAda <|> (Asset <$> pAskedCurrencySymbol <*> pAskedTokenName)
  where
    pAskedAda :: Parser Asset
    pAskedAda = flag' Ada
      (  long "asked-asset-is-ada"
      <> help "The asset asked for is ADA"
      )

    pAskedCurrencySymbol :: Parser CurrencySymbol
    pAskedCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "asked-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the asked asset."
      )

    pAskedTokenName :: Parser TokenName
    pAskedTokenName = option (eitherReader readTokenName)
      (  long "asked-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the asked asset."
      )

pOffered :: Parser Asset
pOffered = pOfferedAda <|> (Asset <$> pOfferedCurrencySymbol <*> pOfferedTokenName)
  where
    pOfferedAda :: Parser Asset
    pOfferedAda = flag' Ada
      (  long "offered-asset-is-ada"
      <> help "The asset being offered is ADA"
      )

    pOfferedCurrencySymbol :: Parser CurrencySymbol
    pOfferedCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "offered-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the offered asset."
      )

    pOfferedTokenName :: Parser TokenName
    pOfferedTokenName = option (eitherReader readTokenName)
      (  long "offered-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the offered asset."
      )

pDatumPrice :: Parser DatumPrice
pDatumPrice = pNewDatum <|> pWeightedPrice
  where
    pNewDatum :: Parser DatumPrice
    pNewDatum = NewDatum <$> pPrice
      where
        pPrice :: Parser Price
        pPrice = fromGHC . (toRational :: Double -> Rational) <$> option auto
          (  long "swap-price"
          <> metavar "DECIMAL"
          <> help "The swap price (asked asset / offered asset)."
          )

    pWeightedPrice :: Parser DatumPrice
    pWeightedPrice = WeightedPrice <$> some pUtxoPriceInfo
      where
        pUtxoPriceInfo :: Parser UtxoPriceInfo
        pUtxoPriceInfo = UtxoPriceInfo
          <$> pAmount
          <*> pPriceNumerator
          <*> pPriceDenominator
        
        pAmount :: Parser Integer
        pAmount = option auto
          (  long "utxo-target-asset-balance"
          <> metavar "INT"
          <> help "How much of the target asset is in this UTxO."
          )
        
        pPriceNumerator :: Parser Integer
        pPriceNumerator = option auto
          (  long "utxo-price-numerator"
          <> metavar "INT"
          <> help "Numerator of asking price for this UTxO."
          )

        pPriceDenominator :: Parser Integer
        pPriceDenominator = option auto
          (  long "utxo-price-denominator"
          <> metavar "INT"
          <> help "Denominator of asking price for this UTxO."
          )