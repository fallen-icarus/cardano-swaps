module CLI.Parsers
(
  parseCommand,
  Command (..),
  AdvancedOption (..),
  Asset (..),
  SwapDatumInfo (..),
) where

import Options.Applicative

import CardanoSwaps
import CLI.Query

data Command 
  = CreateSwapScript !PaymentPubKeyHash !Asset !Asset !FilePath
  | CreateSwapDatum !SwapDatumInfo !FilePath
  | CreateSwapRedeemer !Action !FilePath
  | CreateStakingScript !PaymentPubKeyHash (Maybe Asset) (Maybe Asset)
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | Query !CurrencySymbol !TokenName !CurrencySymbol !TokenName !Network
  | Advanced !AdvancedOption !FilePath

data Asset = Ada | Asset !CurrencySymbol !TokenName

data SwapDatumInfo
  = SwapDatum !Price
  | SwapDatumUtxos !FilePath  -- ^ JSON file for UtxoPriceInfo to be used with calcWeightedPrice
  | SwapDatumUtxosTemplate  -- ^ If a JSON template file is necessary

data AdvancedOption
  = BeaconPolicyId
  | BeaconPolicy
  | BeaconVaultScript

parseCreateSwapScript :: Parser Command
parseCreateSwapScript = 
   CreateSwapScript 
     <$> pOwnerPubKeyHash
     <*> pOffered
     <*> pAsked
     <*> pOutputFile
  where
    pOwnerPubKeyHash :: Parser PaymentPubKeyHash
    pOwnerPubKeyHash = option (eitherReader readPubKeyHash)
      (  long "owner-payment-key-hash" 
      <> metavar "STRING" 
      <> help "The owner's payment key hash."
      )

parseCreateSwapDatum :: Parser Command
parseCreateSwapDatum = 
   CreateSwapDatum 
     <$> (pSwapDatum <|> pSwapUtxoInfo <|> pTemplate) 
     <*> pOutputFile
  where
    pSwapPrice :: Parser Price
    pSwapPrice = fromGHC . (toRational :: Double -> Rational) <$> option auto
      (  long "swap-price"
      <> metavar "DECIMAL"
      <> help "The swap price (asked asset / offered asset)."
      )

    pSwapDatum :: Parser SwapDatumInfo
    pSwapDatum = SwapDatum <$> pSwapPrice

    pSwapUtxoInfo :: Parser SwapDatumInfo
    pSwapUtxoInfo = SwapDatumUtxos <$> strOption
      (  long "calc-swap-price-from-file"
      <> metavar "JSON FILE"
      <> help "JSON file of utxo amounts, price numerators, and price denominators."
      )

    pTemplate :: Parser SwapDatumInfo
    pTemplate = flag' SwapDatumUtxosTemplate
      (  long "swap-price-file-template"
      <> help "Create a template JSON file for use with calc-swap-price-from-file."
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

parseAdvanced :: Parser Command
parseAdvanced = 
   Advanced
     <$> (pBeaconPolicyId <|> pBeaconPolicy <|> pBeaconVaultScript)
     <*> pOutputFile
  where
    pBeaconPolicyId :: Parser AdvancedOption
    pBeaconPolicyId = flag' BeaconPolicyId
      (  long "beacon-policy-id"
      <> help "Output the policy id for the beacons used by the DEX."
      )

    pBeaconPolicy :: Parser AdvancedOption
    pBeaconPolicy = flag' BeaconPolicy
      (  long "beacon-policy-script"
      <> help "Output the beacon policy script."
      )

    pBeaconVaultScript :: Parser AdvancedOption
    pBeaconVaultScript = flag' BeaconVaultScript
      (  long "beacon-vault-script"
      <> help "Output the beacon's deposit vault script."
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
    (info parseQuery (progDesc "Query available swaps for a trading pair.")) <>
  command "advanced"
    (info parseAdvanced (progDesc "Advanced options."))

pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
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