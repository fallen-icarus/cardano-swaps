module CLI.Parsers
(
  parseCommand,
  Command (..)
) where

import Options.Applicative as Opts

import CardanoSwaps
import CLI.Query

data Command 
  = CreateSwapScript !PaymentPubKeyHash !CurrencySymbol !TokenName !CurrencySymbol !TokenName !FilePath
  | CreateSwapDatum !Price !FilePath
  | CreateSwapRedeemer !Action !FilePath
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | Query !CurrencySymbol !TokenName !CurrencySymbol !TokenName !Network

parseCreateSwapScript :: Parser Command
parseCreateSwapScript = 
   CreateSwapScript 
     <$> pOwnerPubKeyHash
     <*> pOfferedCurrencySymbol
     <*> pOfferedTokenName
     <*> pAskedCurrencySymbol
     <*> pAskedTokenName
     <*> pOutputFile
  where
    pOwnerPubKeyHash :: Parser PaymentPubKeyHash
    pOwnerPubKeyHash = option (eitherReader readPubKeyHash)
      (  long "owner-payment-key-hash" 
      <> metavar "STRING" 
      <> help "The owner's payment key hash."
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

parseCreateSwapDatum :: Parser Command
parseCreateSwapDatum = CreateSwapDatum <$> pSwapPrice <*> pOutputFile
  where
    pSwapPrice :: Parser Price
    pSwapPrice = fromGHC . (toRational :: Double -> Rational) <$> option auto
      (  long "swap-price"
      <> metavar "DECIMAL"
      <> help "The swap price (asked asset / offered asset)."
      )


parseCreateSwapRedeemer :: Parser Command
parseCreateSwapRedeemer =
   CreateSwapRedeemer
     <$> (pClose <|> pSwap <|> pInfo <|> fmap UpdatePrices pUpdateSwapPrice)
     <*> pOutputFile
  where
    pClose :: Parser Action
    pClose = flag' Close
      (  long "close-swap"
      <> help "Remove all assets and reference scripts from the swap address."
      )

    pSwap :: Parser Action
    pSwap = flag' Swap
      (  long "swap-assets" 
      <> help "Swap with assets at a swap address."
      )

    pInfo :: Parser Action
    pInfo = flag' Info
      (  long "owner-info"
      <> help "Get the owner info needed to verify contract integrity."
      )

    pUpdateSwapPrice :: Parser Price
    pUpdateSwapPrice = fromGHC . (toRational :: Double -> Rational) <$> option auto
      (  long "update-swap-price"
      <> metavar "DECIMAL"
      <> help "Change the swap price (asked asset / offered asset)."
      )

parseCreateBeaconRedeemer :: Parser Command
parseCreateBeaconRedeemer =
   CreateBeaconRedeemer
     <$> (pMint <|> pBurn)
     <*> pOutputFile
  where
    pMint :: Parser BeaconRedeemer
    pMint = flag' MintBeacon
      (  long "mint-beacon"
      <> help "Mint a beacon to make deposit and create a new swap contract."
      )

    pBurn :: Parser BeaconRedeemer
    pBurn = flag' BurnBeacon
      (  long "burn-beacon"
      <> help "Burn a beacon to close a swap contract and reclaim deposit."
      )

parseQuery :: Parser Command
parseQuery = 
   Query
     <$> pTargetCurrencySymbol
     <*> pTargetTokenName
     <*> pUserCurrencySymbol
     <*> pUserTokenName
     <*> (pMainnet <|> pTestnet)
  where
    pTargetCurrencySymbol :: Parser CurrencySymbol
    pTargetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "target-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the target asset."
      )

    pTargetTokenName :: Parser TokenName
    pTargetTokenName = option (eitherReader readTokenName)
      (  long "target-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the target asset."
      )

    pUserCurrencySymbol :: Parser CurrencySymbol
    pUserCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "user-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the asset you will give to the swap address."
      )

    pUserTokenName :: Parser TokenName
    pUserTokenName = option (eitherReader readTokenName)
      (  long "user-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the asset you will give to the swap address."
      )
    
    pMainnet :: Parser Network
    pMainnet = flag' Mainnet
      (  long "mainnet"
      <> help "Query the Cardano Mainnet using the Koios REST api."
      )
    
    pTestnet :: Parser Network
    pTestnet = PreProdTestnet . BlockfrostApiKey <$> strOption
      (  long "preprod-testnet"
      <> metavar "STRING"
      <> help "Query the Cardano PreProduction Testnet using the Blockfrost REST api and the supplied api key."
      )

parseCommand :: Parser Command
parseCommand = hsubparser $
  command "create-swap-script" 
    (info parseCreateSwapScript (progDesc "Create a unique swap script.")) <>
  command "create-swap-datum" 
    (info parseCreateSwapDatum (progDesc "Create a datum for the swap script.")) <>
  command "create-swap-redeemer"
    (info parseCreateSwapRedeemer (progDesc "Create a redeemer for a swap transaction.")) <>
  command "create-beacon-redeemer"
    (info parseCreateBeaconRedeemer (progDesc "Create a redeemer for the beacon policy.")) <>
  command "query" 
    (info parseQuery (progDesc "Query available swaps for a pair."))

pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )