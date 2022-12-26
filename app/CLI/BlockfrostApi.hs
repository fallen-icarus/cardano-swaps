{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey (..),
  BeaconId (..),
  TargetId (..),
  queryBlockfrost,
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (intersperse,nub,find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Data.Maybe (isJust,fromJust)

import Control.Monad.IO.Class

import CLI.Types (AvailableSwap(..),AvailableAsset(..))

-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Wrapper around CurrencySymbol <> TokenName
newtype BeaconId = BeaconId String 

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId s) = T.pack s

-- | Wrapper around CurrencySymbol <> TokeName
-- The asset being swapped for.
newtype TargetId = TargetId String

instance ToHttpApiData TargetId where
  toQueryParam (TargetId s) = T.pack s

-- | An address that contains a beacon.
-- The response type of the assetAddressList api.
newtype SwapAddress = SwapAddress { unSwapAddress :: String } deriving (Show)

instance FromJSON SwapAddress where
  parseJSON (Object o) = SwapAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData SwapAddress where
  toQueryParam = T.pack . unSwapAddress

-- | The response type of the addressInfo api
data RawSwapInfo = RawSwapInfo
  { address :: String
  , txHash :: String
  , ix :: Integer
  , assets :: [AssetInfo]
  , dataHash :: Maybe String
  , referenceScriptHash :: Maybe String
  } deriving (Show)

instance FromJSON RawSwapInfo where
  parseJSON (Object o) =
    RawSwapInfo
      <$> o .: "address"
      <*> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
      <*> o .: "reference_script_hash"
  parseJSON _ = mzero

-- | Blockfrost does not separate symbol and name with '.'
data AssetInfo = AssetInfo
  { unit :: String  -- ^ CurrencySymbol <> TokenName
  , quantity :: Integer
  } deriving (Show)

instance FromJSON AssetInfo where
  parseJSON (Object o) =
    AssetInfo
      <$> o .: "unit"
      <*> fmap read (o .: "quantity")
  parseJSON _ = mzero

-- | The inline price datum type used in the datumInfoApi
data InlinePriceDatum = InlinePriceDatum
  { numerator :: Integer
  , denominator :: Integer
  } deriving (Show)

instance FromJSON InlinePriceDatum where
  parseJSON (Object o) =
    InlinePriceDatum
      <$> ((o .: "json_value") >>= (.: "fields") >>= (.: "int") . (Vector.! 0))
      <*> ((o .: "json_value") >>= (.: "fields") >>= (.: "int") . (Vector.! 1))
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> Get '[JSON] [SwapAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" SwapAddress
    :> "utxos"
    :> Get '[JSON] [RawSwapInfo]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] InlinePriceDatum

beaconAddressListApi :<|> addressInfoApi :<|> datumInfoApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Query Blockfrost Function
-------------------------------------------------
type ReferenceMap = Map String String
type DatumMap = Map String InlinePriceDatum

queryBlockfrost :: BlockfrostApiKey -> BeaconId -> TargetId -> ClientM [AvailableSwap]
queryBlockfrost apiKey beacon target = do
    addrs <- beaconAddressListApi apiKey beacon
    swapUTxOs <- mapM (addressInfoApi apiKey) addrs
    let rMap = refMap Map.empty addrs swapUTxOs
        allUTxOs = concat swapUTxOs
        nonRefUTxOs = filter (not . isJust . referenceScriptHash) allUTxOs
    datumMap <- datums Map.empty (nub $ map dataHash allUTxOs)
    return $ convert target nonRefUTxOs rMap datumMap

  where
    -- | Create a map from address to reference script TxIxs
    refMap :: Map.Map String String -> [SwapAddress] -> [[RawSwapInfo]] -> ReferenceMap
    refMap rMap [] _ = rMap
    refMap rMap _ [] = rMap
    refMap rMap (SwapAddress addr : as) (ss:sss) = 
      let Just s = find (\z -> isJust $ referenceScriptHash z) ss
          txIx = txHash s <> "#" <> show (ix s)
      in refMap (Map.insert addr txIx rMap) as sss

    -- | Create a map from datumHash to inline datum value
    datums :: Map.Map String InlinePriceDatum 
           -> [Maybe String] 
           -> ClientM (Map.Map String InlinePriceDatum)
    datums datumMap [] = return datumMap
    datums datumMap (d:ds) = case d of
      Nothing -> datums datumMap ds
      Just d' -> do
        i <- datumInfoApi apiKey d'
        datums (Map.insert d' i datumMap) ds

convert :: TargetId -> [RawSwapInfo] -> ReferenceMap -> DatumMap -> [AvailableSwap]
convert _ [] _ _ = []
convert t@(TargetId target) (s:ss) rMap dMap = 
    if target `elem` map unit (assets s)
    then case dataHash s of
      Just d' -> availableSwap (dMap Map.! d') : convert t ss rMap dMap
      -- | Skip if there is an issue
      _ -> convert t ss rMap dMap
    else convert t ss rMap dMap
  where
    txIx :: String
    txIx = txHash s <> "#" <> show (ix s)

    addr :: String 
    addr = address s

    convertAssetInfoToAvailableAsset :: AssetInfo -> AvailableAsset
    convertAssetInfoToAvailableAsset (AssetInfo u q) = 
      if u == "lovelace"
      then AvailableAsset
             { assetPolicyId = u
             , assetTokenName = ""
             , assetQuantity = q
             }
      else AvailableAsset
             { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
             , assetTokenName = drop 56 u
             , assetQuantity = q
             }

    availableSwap :: InlinePriceDatum -> AvailableSwap
    availableSwap inlineD = AvailableSwap
      { swapAddress = addr
      , swapRefScriptTxIx = rMap Map.! addr
      , swapUTxOTxIx = txIx
      , swapUTxOAmount = map convertAssetInfoToAvailableAsset $ assets s
      , swapUTxOPriceNum = numerator inlineD
      , swapUTxOPriceDen = denominator inlineD
      }