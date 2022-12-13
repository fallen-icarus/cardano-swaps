module CLI.Parsers
(
  parseCommand,
  Command (..)
) where

import Options.Applicative as Opts

import CardanoSwaps (PaymentPubKeyHash,CurrencySymbol,TokenName,Price,Action(..),readCurrencySymbol,readPubKeyHash,readTokenName,fromGHC)
import CLI.Query (Network(..),BlockfrostApiKey(..))

data Command 
  = CreateSwapScript !PaymentPubKeyHash !CurrencySymbol !TokenName !CurrencySymbol !TokenName !FilePath
  | CreateDatum !Price !FilePath
  | CreateRedeemer !Action !FilePath
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

parseCreateDatum :: Parser Command
parseCreateDatum = CreateDatum <$> pSwapPrice <*> pOutputFile
  where
    pSwapPrice :: Parser Price
    pSwapPrice = fromGHC . (toRational :: Double -> Rational) <$> option auto
      (  long "swap-price"
      <> metavar "DECIMAL"
      <> help "The swap price (asked asset / offered asset)."
      )


parseCreateRedeemer :: Parser Command
parseCreateRedeemer =
   CreateRedeemer
     <$> (pClose <|> pSwap <|> fmap UpdatePrices pUpdateSwapPrice)
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

    pUpdateSwapPrice :: Parser Price
    pUpdateSwapPrice = fromGHC . (toRational :: Double -> Rational) <$> option auto
      (  long "update-swap-price"
      <> metavar "DECIMAL"
      <> help "Change the swap price (asked asset / offered asset)."
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
  command "create-datum" 
    (info parseCreateDatum (progDesc "Create a datum for the swap script.")) <>
  command "create-redeemer"
    (info parseCreateRedeemer (progDesc "Create a redeemer for a swap transaction.")) <>
  command "query" 
    (info parseQuery (progDesc "Query available swaps for a pair."))

pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )