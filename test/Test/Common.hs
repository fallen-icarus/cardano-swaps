{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Test.Common where

import qualified Data.Map as Map
import Control.Lens hiding (from,index,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash,Value,lovelaceValueOf,from)
import Ledger.Tx.Constraints as Constraints
import qualified Ledger.Tx.Constraints.TxConstraints as Constraints
import Ledger.Tx.Constraints.TxConstraints (TxOutDatum(..))
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Plutus.Script.Utils.Value
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Cardano.Api.Shelley (ProtocolParameters (..))
import qualified Cardano.Api as C
import Cardano.Api hiding (TxOutDatum(..),TxOutDatumInline,TxOutDatumHash,Address,TxId,Value)
import Cardano.Node.Emulator.Params
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidator)

import CardanoSwaps

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
txOutRefWithValue :: Value -> EmulatorTrace TxOutRef
txOutRefWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((ref,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = ref
        | otherwise = findTxId v ys
  return $ findTxId value' xs

toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Maybe (Ledger.ScriptHash) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum maybeScript val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum maybeScript val

instance ToJSON SwapConfig
instance FromJSON SwapConfig

instance ToJSON SwapDatum
instance FromJSON SwapDatum

instance ToJSON SwapRedeemer
instance FromJSON SwapRedeemer

instance ToJSON BeaconRedeemer
instance FromJSON BeaconRedeemer

instance ToJSON DappScripts
instance FromJSON DappScripts

-------------------------------------------------
-- Params
-------------------------------------------------
data OpenSwapAddressParams = OpenSwapAddressParams
  { openSwapAddressBeaconsMinted :: [(TokenName,Integer)]
  , openSwapAddressBeaconRedeemer :: BeaconRedeemer
  , openSwapAddressAddress :: Address
  , openSwapAddressInfo :: [(Maybe SwapDatum, Value)]
  , openSwapAddressAsInline :: Bool
  , openSwapAddressScripts :: DappScripts
  , openSwapAddressWithRefScript :: Bool
  , openSwapRefUTxO :: [(Maybe SwapDatum, Value)]
  } deriving (Generic,ToJSON,FromJSON)

data CloseAddressParams = CloseAddressParams
  { closeBeaconsBurned :: [(TokenName,Integer)]
  , closeBeaconRedeemer :: BeaconRedeemer
  , closeSwapAddress :: Address
  , closeSpecificUtxos :: [(SwapDatum,Value)]
  , closeDappScripts :: DappScripts
  } deriving (Generic,ToJSON,FromJSON)

data UpdateParams = UpdateParams
  { updateSwapAddress :: Address
  , updateSpecificUtxos :: [(SwapDatum,Value)]
  , updateOutputs :: [(Maybe SwapDatum,Value)]
  , updateAsInline :: Bool
  , updateDappScripts :: DappScripts
  } deriving (Generic,ToJSON,FromJSON)

data SwapParams = SwapParams
  { swapAddress :: Address
  , swapSpecificUtxos :: [(SwapDatum,Value)]
  , swapChange :: [(Maybe SwapDatum,Value)]
  , swapChangeDatumAsInline :: Bool
  , swapDappScripts :: DappScripts
  } deriving (Generic,ToJSON,FromJSON)

data ChainSwapParams = ChainSwapParams
  { chainSwapAddresses :: [Address]
  , chainSwapSpecificUTxOs :: [[TxOutRef]]
  , chainSwapChange :: [[(Maybe SwapDatum,Value)]]
  , chainScripts :: [DappScripts]
  , chainWithRefScripts :: Bool
  , chainRefScripts :: [TxOutRef]
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "open-swap-address" OpenSwapAddressParams
  .\/ Endpoint "close-address" CloseAddressParams
  .\/ Endpoint "update" UpdateParams
  .\/ Endpoint "swap" SwapParams
  .\/ Endpoint "chain-swaps" ChainSwapParams

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

testToken3 :: (CurrencySymbol,TokenName)
testToken3 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken3")

testToken4 :: (CurrencySymbol,TokenName)
testToken4 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken4")

testToken5 :: (CurrencySymbol,TokenName)
testToken5 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken5")

testToken6 :: (CurrencySymbol,TokenName)
testToken6 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken6")

testToken7 :: (CurrencySymbol,TokenName)
testToken7 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken7")

testToken8 :: (CurrencySymbol,TokenName)
testToken8 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken8")

testToken9 :: (CurrencySymbol,TokenName)
testToken9 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken9")

testToken10 :: (CurrencySymbol,TokenName)
testToken10 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken10")

testToken11 :: (CurrencySymbol,TokenName)
testToken11 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken11")

testToken12 :: (CurrencySymbol,TokenName)
testToken12 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken12")

testToken13 :: (CurrencySymbol,TokenName)
testToken13 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken13")

testToken14 :: (CurrencySymbol,TokenName)
testToken14 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken14")

testToken15 :: (CurrencySymbol,TokenName)
testToken15 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken15")

testToken16 :: (CurrencySymbol,TokenName)
testToken16 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken16")

testToken17 :: (CurrencySymbol,TokenName)
testToken17 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken17")

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: C.Value
    user1 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000

    user2 :: C.Value
    user2 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
    
    user3 :: C.Value
    user3 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
    
    user4 :: C.Value
    user4 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
    
    user5 :: C.Value
    user5 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000

    user6 :: C.Value
    user6 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
  
    wallets :: [(Wallet,C.Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      ]

benchConfig :: EmulatorConfig
benchConfig = emConfig & params .~ params'
  where 
    params' :: Params
    params' = def{emulatorPParams = pParams'}

    pParams' :: PParams
    pParams' = pParamsFromProtocolParams protoParams

    protoParams :: ProtocolParameters
    protoParams = def{ protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
                                                                        ,executionMemory = 3000000})
                    --  , protocolParamMaxTxSize = 12300
                     }

-------------------------------------------------
-- Trace Models
-------------------------------------------------
openSwapAddress :: OpenSwapAddressParams -> Contract () TraceSchema Text ()
openSwapAddress OpenSwapAddressParams{openSwapAddressScripts=DappScripts{..},..} = do
  let beaconRedeemer = toRedeemer openSwapAddressBeaconRedeemer

      toDatum'
        | openSwapAddressAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      refScript
        | openSwapAddressWithRefScript = Just $ (\(ValidatorHash s) -> ScriptHash s) $ spendingValidatorHash
        | otherwise = Nothing
      
      lookups = plutusV2MintingPolicy beaconPolicy <> plutusV2OtherScript spendingValidator
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          openSwapAddressBeaconsMinted
        )
        -- | Add assets without reference scripts
        <> (foldl'
                (\acc (d,v) -> acc <> mustPayToAddressWith openSwapAddressAddress (fmap toDatum' d) Nothing v)
                mempty
                openSwapAddressInfo
           )
        -- | Add assets with reference scripts
        <> (foldl'
                (\acc (d,v) -> acc <> mustPayToAddressWith openSwapAddressAddress (fmap toDatum' d) refScript v)
                mempty
                openSwapRefUTxO
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swap address created"

closeAddress :: CloseAddressParams -> Contract () TraceSchema Text ()
closeAddress CloseAddressParams{closeDappScripts=DappScripts{..},..} = do
  swapUtxos <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeSwapAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconRedeemer = toRedeemer closeBeaconRedeemer
      closeRedeemer = toRedeemer Close

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs swapUtxos
      
      tx' =
        -- | Burn Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          closeBeaconsBurned
        )
        -- | Spend UTxOs to close.
        <> ( foldl' 
                  (\a (d,v) -> a 
                            <> mustSpendScriptOutputWithMatchingDatumAndValue 
                                 spendingValidatorHash 
                                 (== toDatum d)
                                 (==v) 
                                 closeRedeemer) 
                  mempty 
                  closeSpecificUtxos
           )
        -- | Must be signed by stake pubkey (same as payment for this model)
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swap address closed"

update :: UpdateParams -> Contract () TraceSchema Text ()
update UpdateParams{updateDappScripts=DappScripts{..},..} = do
  swapUtxos <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet updateSwapAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let updateRedeemer = toRedeemer Update

      toDatum'
        | updateAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs swapUtxos
      
      tx' =
        -- | Spend UTxOs to update.
        ( foldl' 
            (\a (d,v) -> a 
                      <> mustSpendScriptOutputWithMatchingDatumAndValue 
                            spendingValidatorHash 
                            (== toDatum d)
                            (==v) 
                            updateRedeemer) 
            mempty 
            updateSpecificUtxos
        )
        -- | Add updated outputs
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith updateSwapAddress (fmap toDatum' d) Nothing v)
              mempty
              updateOutputs
           )
        -- | Must be signed by stake pubkey (same as payment for this model)
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swap(s) updated"

swap :: SwapParams -> Contract () TraceSchema Text ()
swap SwapParams{swapDappScripts=DappScripts{..},..} = do
  swapUtxos <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet swapAddress

  let swapRedeemer = toRedeemer Swap

      toDatum'
        | swapChangeDatumAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs swapUtxos
      
      tx' =
        -- | Spend UTxOs to swap.
        ( foldl' 
            (\a (d,v) -> a 
                      <> mustSpendScriptOutputWithMatchingDatumAndValue 
                            spendingValidatorHash 
                            (== toDatum d)
                            (==v) 
                            swapRedeemer) 
            mempty 
            swapSpecificUtxos
        )
        -- | Add swapd outputs
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith swapAddress (fmap toDatum' d) Nothing v)
              mempty
              swapChange
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swapped UTxO(s)"

chainSwaps :: ChainSwapParams -> Contract () TraceSchema Text ()
chainSwaps ChainSwapParams{..} = do
  swapUTxOs <- Map.unions
           <$> mapM (utxosAt . unsafeFromRight . toCardanoAddressInEra Mainnet) chainSwapAddresses

  let swapRedeemer = toRedeemer Swap
      
      vals = 
        foldl' 
          (\a b -> a <> plutusV2OtherScript (spendingValidator b))
          (plutusV2OtherScript alwaysSucceedValidator)
          chainScripts

      toDatum' = TxOutDatumInline . toDatum
      
      lookups = Constraints.unspentOutputs swapUTxOs <> vals

      tx' = 
        -- | Must reference outputs
        mconcat $ map mustReferenceOutput chainRefScripts

        -- | Must spend all utxos to be swapped
        <> (if chainWithRefScripts then
              ( map (\(ref,xs) -> foldl' (\a i -> 
                          a <>
                          mustSpendScriptOutputWithReference 
                            i
                            swapRedeemer
                            ref
                      ) 
                      mempty
                      xs)
                      (zip chainRefScripts chainSwapSpecificUTxOs)
              )
           else
              ( map (\xs -> foldl' (\a i -> 
                          a <>
                          mustSpendScriptOutput
                            i
                            swapRedeemer
                      ) 
                      mempty
                      xs)
                      chainSwapSpecificUTxOs
              )
           )

        -- | Return change to swap address
        <> (zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) Nothing v)
              mempty
              b)
              chainSwapAddresses
              chainSwapChange
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swaps chained"

-- swapAssets :: SwapAssetsParams -> Contract () TraceSchema Text ()
-- swapAssets SwapAssetsParams{..} = do
--   let swapVal = swapValidator $ convert2SwapConfig swapAddressSwapConfig
--       swapValHash = swapValidatorHash $ convert2SwapConfig swapAddressSwapConfig

--       swapRedeemer = toRedeemer Swap

--       toDatum'
--         | swapChangeDatumAsInline = TxOutDatumInline . toDatum . convert2SwapDatum
--         | otherwise = TxOutDatumHash . toDatum . convert2SwapDatum
  
--   availUtxos <- utxosAt swappableAddress

--   let scriptRef = (\(Just (ref,_)) -> ref)
--                 $ find (isJust . view decoratedTxOutReferenceScript . snd) 
--                 $ Map.toList availUtxos
--       lookups = if swapAll
--                 then Constraints.unspentOutputs availUtxos
--                 else Constraints.unspentOutputs availUtxos
--                   <> plutusV2OtherScript swapVal
--       tx' =
--         -- | Must spend all utxos to be swapped
--         (if swapAll
--          then foldl' 
--                 (\a i -> a <> mustSpendScriptOutputWithReference i swapRedeemer scriptRef)
--                 mempty
--                 (Map.keys availUtxos)
--          else foldl' 
--                 (\a (d,v) -> a 
--                         <> mustSpendScriptOutputWithMatchingDatumAndValue 
--                               swapValHash 
--                               (== toDatum (convert2SwapDatum d)) 
--                               (==v) 
--                               swapRedeemer) 
--                 mempty 
--                 swapUtxos
--         )
--         -- | Must output change to script address
--         <> (foldl'
--               (\acc (d,v) -> acc <> mustPayToAddressWith swappableAddress (fmap toDatum' d) Nothing v)
--               mempty
--               swapChange
--            )
  
--   ledgerTx <- submitTxConstraintsWith @Void lookups tx'
--   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--   logInfo @String "Created a live swap address"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    openSwapUtxo' = endpoint @"open-swap-address" openSwapAddress
    closeAddress' = endpoint @"close-address" closeAddress
    update' = endpoint @"update" update
    swap' = endpoint @"swap" swap
    chainSwaps' = endpoint @"chain-swaps" chainSwaps
    choices = 
      [ openSwapUtxo'
      , closeAddress'
      , update'
      , swap'
      , chainSwaps'
      ]