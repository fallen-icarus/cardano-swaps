module CLI.Types where

import Data.ByteString (ByteString)

import CardanoSwaps

-- | Used for most commands
data Asset = Ada | Asset !CurrencySymbol !TokenName

-- | Used to generate beacon token name
data RawAsset = RawAda | RawAsset !ByteString !ByteString

-- | Used in runQuery
newtype QueryAsset = QueryAsset { unQueryAsset :: (String,String) }

-- | For when saving to file is optional
data Output = StdOut | File !FilePath

data SwapDatumInfo
  = SwapDatum !Price
  | SwapDatumUtxos !FilePath  -- ^ JSON file for UtxoPriceInfo to be used with calcWeightedPrice
  | SwapDatumUtxosTemplate  -- ^ If a JSON template file is necessary

data Command
  = SwapScript SwapScriptCmd
  | StakingScript StakingScriptCmd
  | Beacon BeaconCmd
  | QueryAvailableSwaps !RawAsset !RawAsset !Network !Output

data SwapScriptCmd
  = CreateSwapScript !PaymentPubKeyHash !Asset !Asset !FilePath
  | CreateSwapDatum !SwapDatumInfo !FilePath
  | CreateSwapRedeemer !Action !FilePath

data StakingScriptCmd
  = CreateStakingScript !PaymentPubKeyHash (Maybe Asset) (Maybe Asset) !FilePath
  | CreateStakingRedeemer !FilePath

data BeaconCmd
  = GenerateBeaconTokenName !RawAsset !RawAsset !Output
  | ExportBeaconPolicyId !Output
  | CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | CreateBeaconDatum !FilePath  -- ^ For use with the beacon vault script
  | ExportBeaconPolicyScript !FilePath
  | ExportBeaconVaultScript !FilePath

data Network
  -- | Uses the Koios REST api. 
  = Mainnet
  -- | Koios REST api does not support the PreProduction Testnet.
  --   Until they do, Blockfrost will be used for querying the PreProduction Testnet.
  | PreProdTestnet String  -- ^ Api key

-- | Type that captures all info a user needs to interact with swap scripts
data AvailableSwap = AvailableSwap
  { swapAddress :: String
  , swapRefScriptTxIx :: String
  , swapUTxOTxIx :: String
  , swapUTxOAmount :: [AvailableAsset]
  , swapUTxOPriceNum :: Integer
  , swapUTxOPriceDen :: Integer
  } deriving (Show)

data AvailableAsset = AvailableAsset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: Integer
  } deriving (Show)