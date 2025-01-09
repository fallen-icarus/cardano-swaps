{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Query.Koios
  (
    queryAllSwapsByTradingPair
  , queryAllSwapsByOffer
  , queryAllSwapsByAsk
  , queryOwnSwaps
  , queryOwnSwapsByBeacon
  , submitTx
  , evaluateTx
  , getParams
  , queryPersonalAddress
  ) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortOn)

import CLI.Types
import CardanoSwaps.Utils
import qualified CardanoSwaps.OneWaySwap as OneWay
import qualified CardanoSwaps.TwoWaySwap as TwoWay

-------------------------------------------------
-- Core Types
-------------------------------------------------
instance FromJSON Asset where
  parseJSON (Object o) =
    Asset
      <$> o .: "policy_id"
      <*> o .: "asset_name"
      <*> o .: "quantity"
  parseJSON _ = mzero

newtype AssetList = AssetList [(CurrencySymbol,TokenName)]

instance ToJSON AssetList where
  toJSON (AssetList xs) = 
    object [ "_asset_list" .= 
             map (\(currSym,tokName) -> [T.pack $ show currSym,T.pack $ drop 2 $ show tokName]) xs
           , "_extended" .= True
           ]

data KoiosUTxO = KoiosUTxO
  { koiosUtxoAddress :: UserAddress
  , koiosUtxoTxHash :: Text
  , koiosUtxoOutputIndex :: Integer
  , koiosUtxoLovelaceValue :: Text
  , koiosUtxoInlineDatum :: Maybe Value
  , koiosUtxoDatumHash :: Maybe Text
  , koiosUtxoReferenceScriptHash :: Maybe Text
  , koiosUtxoAssetList :: [Asset]
  } deriving (Show)

instance FromJSON KoiosUTxO where
  parseJSON (Object o) =
    KoiosUTxO
      <$> (UserAddress <$> o .: "address")
      <*> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "value"
      <*> (o .: "inline_datum" >>= 
            maybe (pure Nothing) (withObject "inline_datum" $ \o' -> o' .: "value"))
      <*> o .: "datum_hash"
      <*> (o .: "reference_script" >>= 
            maybe (pure Nothing) (withObject "reference_script" $ \o' -> o' .: "hash"))
      <*> o .: "asset_list"
  parseJSON _ = mzero

newtype AddressList = AddressList { unAddressList :: [UserAddress] } deriving (Show)

instance ToJSON AddressList where
  toJSON (AddressList as) = 
    object [ "_addresses" .= map (\(UserAddress addr) -> addr) as 
           , "_extended" .= True
           ]

newtype SubmitTxCBOR = SubmitTxCBOR TxCBOR

instance ToJSON SubmitTxCBOR where
  toJSON (SubmitTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("submitTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

newtype EvaluateTxCBOR = EvaluateTxCBOR TxCBOR

instance ToJSON EvaluateTxCBOR where
  toJSON (EvaluateTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("evaluateTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

-------------------------------------------------
-- Koios Api
-------------------------------------------------
type KoiosApi
  =     ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value

  :<|>  "cli_protocol_params"
     :> Get '[JSON] Value

  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] AssetList
     :> Post '[JSON] [KoiosUTxO]

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] AddressList
     :> Post '[JSON] [KoiosUTxO]

submitApi :<|> evaluateApi :<|> paramsApi :<|> assetUTxOsApi :<|> addressUTxOsApi = client api
  where
    api :: Proxy KoiosApi
    api = Proxy

-------------------------------------------------
-- Koios Query Functions
-------------------------------------------------
queryAllSwapsByTradingPair :: OfferAsset -> AskAsset -> ClientM [SwapUTxO]
queryAllSwapsByTradingPair o@(OfferAsset offer@(currSym,_)) a@(AskAsset ask) = do
  let oneWayBeacon = (OneWay.beaconCurrencySymbol, OneWay.genPairBeaconName o a)
      twoWayBeacon = (TwoWay.beaconCurrencySymbol, TwoWay.genPairBeaconName offer ask)
      offerFilter = if currSym == "" then Nothing else Just $ "cs." <> assetToQueryParam offer
  oneWayUTxOs <- 
    assetUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      offerFilter
      (AssetList [oneWayBeacon])
  twoWayUTxOs <- 
    assetUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      offerFilter
      (AssetList [twoWayBeacon])
  return $ sortOn (prices o a) $ map convertToSwapUTxO $ oneWayUTxOs <> twoWayUTxOs

queryAllSwapsByOffer :: OfferAsset -> ClientM [SwapUTxO]
queryAllSwapsByOffer offer@(OfferAsset asset@(currSym,_))  = do
  let oneWayOfferBeaconName = OneWay.genOfferBeaconName offer
      twoWayOfferBeaconName = TwoWay.genAssetBeaconName asset
      oneWayBeacon = (OneWay.beaconCurrencySymbol, oneWayOfferBeaconName)
      twoWayBeacon = (TwoWay.beaconCurrencySymbol, twoWayOfferBeaconName)
      offerFilter = if currSym == "" then Nothing else Just $ "cs." <> assetToQueryParam asset
  oneWayUTxOs <- 
    assetUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      offerFilter
      (AssetList [oneWayBeacon])
  twoWayUTxOs <- 
    assetUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      offerFilter
      (AssetList [twoWayBeacon])
  return $ map convertToSwapUTxO $ oneWayUTxOs <> twoWayUTxOs

queryAllSwapsByAsk :: AskAsset -> ClientM [SwapUTxO]
queryAllSwapsByAsk ask@(AskAsset asset)  = do
  let oneWayAskBeaconName = OneWay.genAskBeaconName ask
      twoWayAskBeaconName = TwoWay.genAssetBeaconName asset
      oneWayBeacon = (OneWay.beaconCurrencySymbol, oneWayAskBeaconName)
      twoWayBeacon = (TwoWay.beaconCurrencySymbol, twoWayAskBeaconName)
  oneWayUTxOs <- 
    assetUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      Nothing
      (AssetList [oneWayBeacon])
  twoWayUTxOs <- 
    assetUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      Nothing
      (AssetList [twoWayBeacon])
  return $ map convertToSwapUTxO $ oneWayUTxOs <> twoWayUTxOs

submitTx :: TxCBOR -> ClientM Value
submitTx = submitApi . SubmitTxCBOR

evaluateTx :: TxCBOR -> ClientM Value
evaluateTx = evaluateApi . EvaluateTxCBOR

getParams :: ClientM Value
getParams = paramsApi

queryPersonalAddress :: UserAddress -> ClientM [PersonalUTxO]
queryPersonalAddress addr =
  sortOn personalTxHash . map convertToPersonalUTxO <$> 
    addressUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false" 
      Nothing
      (AddressList [addr])  

queryOwnSwaps :: UserAddress -> ClientM [SwapUTxO]
queryOwnSwaps addr = do
  utxos <- 
    addressUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      Nothing
      (AddressList [addr])
  return $ map convertToSwapUTxO utxos

queryOwnSwapsByBeacon
  :: UserAddress 
  -> CurrencySymbol 
  -> TokenName 
  -> ClientM [SwapUTxO]
queryOwnSwapsByBeacon addr beaconId beaconName = do
  let beacon = (beaconId, beaconName)
  utxos <- 
    addressUTxOsApi 
      "is_spent,tx_hash,tx_index,address,value,datum_hash,inline_datum,asset_list,reference_script"
      "eq.false"
      (Just $ "cs." <> assetToQueryParam beacon)
      (AddressList [addr])
  return $ map convertToSwapUTxO utxos

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
convertToPersonalUTxO :: KoiosUTxO -> PersonalUTxO
convertToPersonalUTxO KoiosUTxO{..} =
  PersonalUTxO
    { personalTxHash = koiosUtxoTxHash
    , personalOutputIndex = koiosUtxoOutputIndex
    , personalValue = Asset "" "" koiosUtxoLovelaceValue : koiosUtxoAssetList
    , personalDatumHash = koiosUtxoDatumHash
    , personalReferenceScriptHash = koiosUtxoReferenceScriptHash
    }

convertToSwapUTxO :: KoiosUTxO -> SwapUTxO
convertToSwapUTxO KoiosUTxO{..} = 
    SwapUTxO
      { swapAddress = koiosUtxoAddress
      , swapTxHash = koiosUtxoTxHash 
      , swapOutputIndex = koiosUtxoOutputIndex
      , swapValue = Asset "" "" koiosUtxoLovelaceValue : koiosUtxoAssetList
      , swapDatum = datum
      }
  where
    oneWaySwapDatum = (decodeDatum @OneWay.SwapDatum) =<< koiosUtxoInlineDatum
    twoWaySwapDatum = (decodeDatum @TwoWay.SwapDatum) =<< koiosUtxoInlineDatum
    datum = case (oneWaySwapDatum,twoWaySwapDatum) of
      (Nothing,Nothing) -> Nothing
      (Just oneDatum,Nothing) -> Just $ OneWayDatum oneDatum
      (Nothing,Just twoDatum) -> Just $ TwoWayDatum twoDatum
      _ -> Prelude.error "The impossible happened" -- The UTxO can't have BOTH types of datums.

assetToQueryParam :: AssetConfig -> Text
assetToQueryParam (currSym,tokName) = 
  let policyId = T.pack $ show currSym
      assetName = T.pack $ showTokenName tokName
  in "[{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}]"

-- Get the prices from a SwapUTxO based on the swap direction. This should only be used on beacon
-- query results since they are guaranteed to have a datum.
prices :: OfferAsset -> AskAsset -> SwapUTxO -> PlutusRational
prices _ _ SwapUTxO{swapDatum = Just (OneWayDatum OneWay.SwapDatum{..})} = swapPrice
prices (OfferAsset offer) (AskAsset ask) SwapUTxO{swapDatum = Just (TwoWayDatum TwoWay.SwapDatum{..})}
  | offer < ask = asset1Price
  | otherwise = asset2Price
prices _ _ _ = error "CLI.Query.Koios.prices used on swap without datum"
