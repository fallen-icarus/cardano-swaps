{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.Query.Koios
  (
    queryAllSwapsByTradingPair,
    queryAllSwapsByOffer,
    queryOwnSwaps,
    queryOwnSwapsByTradingPair,
    queryOwnSwapsByOffer,
    queryPersonalAddress,
    submitTx
  ) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find,sortOn)
import Data.Maybe (isJust)

import CLI.Types
import CardanoSwaps

-------------------------------------------------
-- Core Types
-------------------------------------------------
instance ToHttpApiData CurrencySymbol where
  toQueryParam currSym = T.pack $ show currSym

instance ToHttpApiData TokenName where
  toQueryParam tokName = T.pack $ drop 2 $ show tokName

instance FromJSON UserAddress where
  parseJSON (Object o) = UserAddress <$> o .: "payment_address"
  parseJSON _ = mzero

newtype AddressList = AddressList { unAddressList :: [UserAddress] } deriving (Show)

instance ToJSON AddressList where
  toJSON (AddressList as) = object [ "_addresses" .= map (\(UserAddress addr) -> addr) as ]

data AddressInfo = AddressInfo
  { address :: UserAddress
  , utxoSet :: [AddressUTxO]
  } deriving (Show)

instance FromJSON AddressInfo where
  parseJSON (Object o) = 
    AddressInfo
      <$> (UserAddress <$> o .: "address")
      <*> o .: "utxo_set"
  parseJSON _ = mzero

data AddressUTxO = AddressUTxO
  { utxoTxHash :: Text
  , utxoOutputIndex :: Integer
  , utxoLovelaceValue :: Text
  , utxoInlineDatum :: Maybe Value
  , utxoDatumHash :: Maybe Text
  , utxoReferenceScriptHash :: Maybe Text
  , utxoAssetList :: [Asset]
  } deriving (Show)

instance FromJSON Asset where
  parseJSON (Object o) =
    Asset
      <$> o .: "policy_id"
      <*> o .: "asset_name"
      <*> o .: "quantity"
  parseJSON _ = mzero

instance FromJSON AddressUTxO where
  parseJSON (Object o) =
    AddressUTxO
      <$> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "value"
      <*> (o .: "inline_datum" >>= maybe (pure Nothing) (\(Object o') -> o' .: "value"))
      <*> o .: "datum_hash"
      <*> (o .: "reference_script" >>= maybe (pure Nothing) (\(Object o') -> o' .: "hash"))
      <*> o .: "asset_list"
  parseJSON _ = mzero

-------------------------------------------------
-- Koios Api
-------------------------------------------------
type KoiosApi
  =    "asset_addresses"
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "_asset_name" TokenName
     :> QueryParam' '[Required] "select" Text
     :> Get '[JSON] [UserAddress]

  :<|> "policy_asset_addresses"
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "select" Text
     :> Get '[JSON] [UserAddress]

  :<|> "address_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] AddressList
     :> Post '[JSON] [AddressInfo]

  :<|> "submittx"
     :> ReqBody '[CBOR] TxCBOR
     :> Post '[JSON] Text

pairBeaconAddressListApi 
  :<|> offerBeaconAddressListApi 
  :<|> addressInfoApi 
  :<|> submitApi = client api
  where
    api :: Proxy KoiosApi
    api = Proxy

-------------------------------------------------
-- Koios Query Functions
-------------------------------------------------
queryAllSwapsByTradingPair :: CurrencySymbol -> TokenName -> AssetConfig -> ClientM [SwapUTxO]
queryAllSwapsByTradingPair beaconSym beaconTokName (assetId,assetName) = do
  let beacon = (T.pack $ show beaconSym, T.pack $ drop 2 $ show beaconTokName)
      offer = (T.pack $ show assetId, T.pack $ drop 2 $ show assetName)
  addrs <- pairBeaconAddressListApi beaconSym beaconTokName "payment_address"
  utxos <- addressInfoApi "address,utxo_set" (AddressList addrs)  
  return $ sortOn (fmap swapPrice . swapDatum) 
         $ concatMap (convertTradingPairToSwapUTxO beacon offer) utxos

queryAllSwapsByOffer :: CurrencySymbol -> ClientM [SwapUTxO]
queryAllSwapsByOffer beaconSym = do
  addrs <- offerBeaconAddressListApi beaconSym "payment_address"
  utxos <- addressInfoApi "address,utxo_set" (AddressList addrs)  
  return $ sortOn swapTxHash $ concatMap (convertOfferToSwapUTxO (T.pack $ show beaconSym)) utxos

queryOwnSwaps :: UserAddress -> ClientM [SwapUTxO]
queryOwnSwaps addr = do
  utxos <- addressInfoApi "address,utxo_set" (AddressList [addr])
  return $ concatMap convertAllToSwapUTxO utxos

queryOwnSwapsByTradingPair :: UserAddress -> CurrencySymbol -> TokenName -> ClientM [SwapUTxO]
queryOwnSwapsByTradingPair addr beaconSym beaconTokName = do
  let beacon = (T.pack $ show beaconSym, T.pack $ drop 2 $ show beaconTokName)
  utxos <- addressInfoApi "address,utxo_set" (AddressList [addr])  
  return $ sortOn (fmap swapPrice . swapDatum) 
         $ concatMap (convertAllTradingPairToSwapUTxO beacon) utxos

queryOwnSwapsByOffer :: UserAddress -> CurrencySymbol -> ClientM [SwapUTxO]
queryOwnSwapsByOffer addr beaconSym = do
  utxos <- addressInfoApi "address,utxo_set" (AddressList [addr])  
  return $ sortOn swapTxHash $ concatMap (convertOfferToSwapUTxO (T.pack $ show beaconSym)) utxos

queryPersonalAddress :: UserAddress -> ClientM [PersonalUTxO]
queryPersonalAddress addr =
  sortOn personalTxHash . concatMap convertToPersonalUTxO <$> 
    addressInfoApi "address,utxo_set" (AddressList [addr])  

submitTx :: TxCBOR -> ClientM Text
submitTx = submitApi

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
hasAsset :: (Text,Text) -> [Asset] -> Bool
hasAsset (oSym,oName) = isJust . find (\(Asset sym name _) -> sym == oSym && name == oName)

hasBeacon :: Text -> [Asset] -> Bool
hasBeacon oSym = isJust . find (\(Asset sym _ _ ) -> sym == oSym)

-- | This will return only the UTxOs for the target trading pair with the offer asset..
convertTradingPairToSwapUTxO :: (Text,Text) -> (Text,Text) -> AddressInfo -> [SwapUTxO]
convertTradingPairToSwapUTxO _ _ (AddressInfo _ []) = []
convertTradingPairToSwapUTxO beacon offer@(oSym,_) (AddressInfo addr (AddressUTxO{..}:xs))
  -- | If lovelace is offered, only check for the beacon.
  | oSym == "" && hasAsset beacon utxoAssetList = info : next
  -- | Otherwise check for both the beacon and the offered asset.
  | hasAsset beacon utxoAssetList && hasAsset offer utxoAssetList = info : next
  -- | If the above two checks failed, this UTxO is for another trading pair.
  | otherwise = next
  where
    swapDatum = join $ fmap decodeDatum utxoInlineDatum
    next = convertTradingPairToSwapUTxO beacon offer (AddressInfo addr xs)
    info = SwapUTxO
      { swapAddress = addr
      , swapTxHash = utxoTxHash 
      , swapOutputIndex = utxoOutputIndex
      , swapValue = (Asset "" "" utxoLovelaceValue) : utxoAssetList
      , swapDatum = swapDatum
      }

-- | This will return any swap UTxOs with the specified offer asset.
convertOfferToSwapUTxO :: Text -> AddressInfo -> [SwapUTxO]
convertOfferToSwapUTxO _ (AddressInfo _ []) = []
convertOfferToSwapUTxO beaconSym (AddressInfo addr (AddressUTxO{..}:xs))
  -- | If the UTxO has one of the beacon's, it is should be returned.
  | hasBeacon beaconSym utxoAssetList = info : next
  -- | Otherwise, this UTxO is for another trading pair.
  | otherwise = next
  where
    swapDatum = join $ fmap decodeDatum utxoInlineDatum
    next = convertOfferToSwapUTxO beaconSym (AddressInfo addr xs)
    info = SwapUTxO
      { swapAddress = addr
      , swapTxHash = utxoTxHash 
      , swapOutputIndex = utxoOutputIndex
      , swapValue = (Asset "" "" utxoLovelaceValue) : utxoAssetList
      , swapDatum = swapDatum
      }

-- | This will return all swap UTxOs.
convertAllToSwapUTxO :: AddressInfo -> [SwapUTxO]
convertAllToSwapUTxO (AddressInfo _ []) = []
convertAllToSwapUTxO (AddressInfo addr (AddressUTxO{..}:xs)) = info : next
  where
    swapDatum = join $ fmap decodeDatum utxoInlineDatum
    next = convertAllToSwapUTxO (AddressInfo addr xs)
    info = SwapUTxO
      { swapAddress = addr
      , swapTxHash = utxoTxHash 
      , swapOutputIndex = utxoOutputIndex
      , swapValue = (Asset "" "" utxoLovelaceValue) : utxoAssetList
      , swapDatum = swapDatum
      }

-- | This will return all the UTxOs for the target trading pair.
convertAllTradingPairToSwapUTxO :: (Text,Text) -> AddressInfo -> [SwapUTxO]
convertAllTradingPairToSwapUTxO _ (AddressInfo _ []) = []
convertAllTradingPairToSwapUTxO beacon (AddressInfo addr (AddressUTxO{..}:xs))
  -- | Check for the beacon.
  | hasAsset beacon utxoAssetList = info : next
  -- | If the above check failed, this UTxO is for another trading pair.
  | otherwise = next
  where
    swapDatum = join $ fmap decodeDatum utxoInlineDatum
    next = convertAllTradingPairToSwapUTxO beacon (AddressInfo addr xs)
    info = SwapUTxO
      { swapAddress = addr
      , swapTxHash = utxoTxHash 
      , swapOutputIndex = utxoOutputIndex
      , swapValue = (Asset "" "" utxoLovelaceValue) : utxoAssetList
      , swapDatum = swapDatum
      }

convertToPersonalUTxO :: AddressInfo -> [PersonalUTxO]
convertToPersonalUTxO (AddressInfo _ []) = []
convertToPersonalUTxO (AddressInfo addr (AddressUTxO{..}:xs)) = info : next
  where
    next = convertToPersonalUTxO (AddressInfo addr xs)
    info = PersonalUTxO
      { personalTxHash = utxoTxHash
      , personalOutputIndex = utxoOutputIndex
      , personalValue = (Asset "" "" utxoLovelaceValue) : utxoAssetList
      , personalDatumHash = utxoDatumHash
      , personalReferenceScriptHash = utxoReferenceScriptHash
      }

