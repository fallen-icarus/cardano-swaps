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

module Test.Common where

import qualified Data.Map as Map
import Control.Lens
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Playground.Contract (ToSchema)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Plutus.Script.Utils.V2.Generators

import CardanoSwaps

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Maybe ScriptHash -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum maybeRef val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum maybeRef val

data SwapConfig' = SwapConfig'
  { swapOffer' :: (CurrencySymbol,TokenName)
  , swapAsk' :: (CurrencySymbol,TokenName)
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

convert2SwapConfig :: SwapConfig' -> SwapConfig
convert2SwapConfig (SwapConfig' offer ask) = SwapConfig offer ask

data SwapDatum' = SwapDatum'
  { swapPrice' :: Price
  , swapBeacon' :: Maybe CurrencySymbol
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

convert2SwapDatum :: SwapDatum' -> SwapDatum
convert2SwapDatum (SwapDatum' price beaconSym) = SwapDatum price beaconSym

-- | Data type to specify which reference script to use.
data RefScript = None | AlwaysSucceed | Proper
  deriving (Generic,ToJSON,FromJSON,ToSchema)

-------------------------------------------------
-- Trace Configs
-------------------------------------------------
data CreateLiveSwapAddressParams = CreateLiveSwapAddressParams
  { beaconsMinted :: [(TokenName,Integer)]
  , useMintRedeemer :: Bool
  , createLiveBeaconSwapConfig :: SwapConfig'
  , createLiveAddressSwapConfig :: SwapConfig'
  , createLiveAddress :: Address
  , createLiveRefScript :: RefScript
  , createLiveRefScriptUtxo :: (Maybe SwapDatum',Value)
  , createLiveInitialPositions :: [(Maybe SwapDatum',Value)]
  , createLiveDatumsAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data BurnBeaconParams = BurnBeaconParams
  { beaconsBurned :: [(TokenName,Integer)]
  , useBurnRedeemer :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data CloseAddressParams = CloseAddressParams
  { closeBeaconsBurned :: [(TokenName,Integer)]
  , closeBeaconSwapConfig :: SwapConfig'
  , closeAddressSwapConfig :: SwapConfig'
  , closeAddress :: Address
  , closeAll :: Bool  -- ^ This will overshadow closeSpecificUtxos.
  , closeSpecificUtxos :: [(SwapDatum',Value)]
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data UpdateSwapsParams = UpdateSwapsParams
  { updateAddressSwapConfig :: SwapConfig'
  , updateAddress :: Address
  , updateAll :: Bool
  , updateSpecificUtxos :: [(SwapDatum',Value)]
  , updatedOutputs :: [(Maybe SwapDatum',Value)]
  , updateDatumsAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data SwapAssetsParams = SwapAssetsParams
  { swapAddressSwapConfig :: SwapConfig'
  , swappableAddress :: Address
  , swapUtxos :: [(SwapDatum',Value)]
  , swapChange :: [(Maybe SwapDatum',Value)]
  , swapChangeDatumAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

type TraceSchema =
      Endpoint "create-live-swap-address" CreateLiveSwapAddressParams
  .\/ Endpoint "burn-beacons" BurnBeaconParams
  .\/ Endpoint "close-live-address" CloseAddressParams
  .\/ Endpoint "update-swaps" UpdateSwapsParams
  .\/ Endpoint "swap-assets" SwapAssetsParams

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

swapConfig1 :: SwapConfig'
swapConfig1 = SwapConfig'
  { swapOffer' = (adaSymbol,adaToken)
  , swapAsk' = testToken1
  }

swapConfig2 :: SwapConfig'
swapConfig2 = SwapConfig'
  { swapOffer' = testToken1
  , swapAsk' = testToken2
  }

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
         <> singleton (beaconSymbol $ convert2SwapConfig swapConfig1) adaToken 5
         <> (uncurry singleton testToken2) 100

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
         <> singleton (beaconSymbol $ convert2SwapConfig swapConfig2) adaToken 5
    
    user3 :: Value
    user3 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
    
    user4 :: Value
    user4 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
  
    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      ]

refScriptDeposit :: Value
refScriptDeposit = lovelaceValueOf 23_000_000

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createLiveSwapAddress :: CreateLiveSwapAddressParams -> Contract () TraceSchema Text ()
createLiveSwapAddress CreateLiveSwapAddressParams {..} = do
  let beaconPolicy' = beaconPolicy $ convert2SwapConfig createLiveBeaconSwapConfig
      beaconPolicyHash = mintingPolicyHash beaconPolicy'
      beaconRedeemer
        | useMintRedeemer = toRedeemer MintBeacon
        | otherwise = toRedeemer BurnBeacon

      swapVal = swapValidator $ convert2SwapConfig createLiveAddressSwapConfig
      swapValHash = swapValidatorHash $ convert2SwapConfig createLiveAddressSwapConfig

      maybeRef = case createLiveRefScript of
        None -> Nothing
        AlwaysSucceed -> Just $ (\(ValidatorHash s) -> ScriptHash s) $ alwaysSucceedValidatorHash
        Proper -> Just $ (\(ValidatorHash s) -> ScriptHash s) $ swapValHash

      toDatum'
        | createLiveDatumsAsInline = TxOutDatumInline . toDatum . convert2SwapDatum
        | otherwise = TxOutDatumHash . toDatum . convert2SwapDatum
  
      lookups = plutusV2MintingPolicy beaconPolicy'
             <> plutusV2OtherScript alwaysSucceedValidator
             <> plutusV2OtherScript swapVal
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          beaconsMinted
        )
        -- | Must store reference script
        <> mustPayToAddressWith 
            createLiveAddress 
            (fmap toDatum' $ fst createLiveRefScriptUtxo) 
            maybeRef 
            (snd createLiveRefScriptUtxo)
        -- | Add positions
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith createLiveAddress (fmap toDatum' d) Nothing v)
              mempty
              createLiveInitialPositions
           )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Created a live swap address"

burnBeacons :: BurnBeaconParams -> Contract () TraceSchema Text ()
burnBeacons BurnBeaconParams{..} = do
  let swapCfg = convert2SwapConfig swapConfig1
    
      beaconPolicy' = beaconPolicy swapCfg
      beaconPolicyHash = mintingPolicyHash beaconPolicy'
      beaconRedeemer
        | useBurnRedeemer = toRedeemer BurnBeacon
        | otherwise = toRedeemer MintBeacon
      
      lookups = plutusV2MintingPolicy beaconPolicy'
      tx' =
        -- | Burn beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          beaconsBurned
        )
      
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Burned beacon(s)"

closeLiveAddress :: CloseAddressParams -> Contract () TraceSchema Text ()
closeLiveAddress CloseAddressParams{..} = do
  let beaconPolicy' = beaconPolicy $ convert2SwapConfig closeBeaconSwapConfig
      beaconPolicyHash = mintingPolicyHash beaconPolicy'
      beaconRedeemer = toRedeemer BurnBeacon

      swapVal = swapValidator $ convert2SwapConfig closeAddressSwapConfig
      swapValHash = swapValidatorHash $ convert2SwapConfig closeAddressSwapConfig

      closeRedeemer = toRedeemer Close
      
  swapUtxos <- utxosAt closeAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let scriptRef = (\(Just (ref,_)) -> ref)
                $ find (isJust . view decoratedTxOutReferenceScript . snd) 
                $ Map.toList swapUtxos
      lookups = if closeAll
                then Constraints.unspentOutputs swapUtxos
                  <> plutusV2MintingPolicy beaconPolicy'
                else Constraints.unspentOutputs swapUtxos
                  <> plutusV2MintingPolicy beaconPolicy'
                  <> plutusV2OtherScript swapVal
      tx' = 
        -- | Must burn beacon
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          closeBeaconsBurned
        )
        -- | Must spend all utxos to be closed
        <> (if closeAll
           then foldl' 
                  (\a i -> a <> mustSpendScriptOutputWithReference i closeRedeemer scriptRef)
                  mempty
                  (Map.keys swapUtxos)
           else foldl' 
                  (\a (d,v) -> a 
                            <> mustSpendScriptOutputWithMatchingDatumAndValue 
                                 swapValHash 
                                 (== toDatum (convert2SwapDatum d)) 
                                 (==v) 
                                 closeRedeemer) 
                  mempty 
                  closeSpecificUtxos
           )
        -- | Must be signed by stake pubkey (same as payment for this model)
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed a swap address"

updateSwaps :: UpdateSwapsParams -> Contract () TraceSchema Text ()
updateSwaps UpdateSwapsParams{..} = do
  let swapVal = swapValidator $ convert2SwapConfig updateAddressSwapConfig
      swapValHash = swapValidatorHash $ convert2SwapConfig updateAddressSwapConfig

      updateRedeemer = toRedeemer Update

      toDatum'
        | updateDatumsAsInline = TxOutDatumInline . toDatum . convert2SwapDatum
        | otherwise = TxOutDatumHash . toDatum . convert2SwapDatum
      
  swapUtxos <- utxosAt updateAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let scriptRef = (\(Just (ref,_)) -> ref)
                $ find (isJust . view decoratedTxOutReferenceScript . snd) 
                $ Map.toList swapUtxos
      lookups = if updateAll
                then Constraints.unspentOutputs swapUtxos
                else Constraints.unspentOutputs swapUtxos
                  <> plutusV2OtherScript swapVal
      tx' = 
        -- | Must spend all utxos to be updated
        (if updateAll
         then foldl' 
                (\a i -> a <> mustSpendScriptOutputWithReference i updateRedeemer scriptRef)
                mempty
                (Map.keys swapUtxos)
         else foldl' 
                (\a (d,v) -> a 
                        <> mustSpendScriptOutputWithMatchingDatumAndValue 
                              swapValHash 
                              (== toDatum (convert2SwapDatum d)) 
                              (==v) 
                              updateRedeemer) 
                mempty 
                updateSpecificUtxos
        )
        -- | Must be signed by stake pubkey (same as payment for this model)
        <> mustBeSignedBy userPubKeyHash
        -- | Add updated outputs
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith updateAddress (fmap toDatum' d) Nothing v)
              mempty
              updatedOutputs
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Updated swaps"

swapAssets :: SwapAssetsParams -> Contract () TraceSchema Text ()
swapAssets SwapAssetsParams{..} = do
  let swapVal = swapValidator $ convert2SwapConfig swapAddressSwapConfig
      swapValHash = swapValidatorHash $ convert2SwapConfig swapAddressSwapConfig

      swapRedeemer = toRedeemer Swap

      toDatum'
        | swapChangeDatumAsInline = TxOutDatumInline . toDatum . convert2SwapDatum
        | otherwise = TxOutDatumHash . toDatum . convert2SwapDatum
  
  availUtxos <- utxosAt swappableAddress

  let lookups = Constraints.unspentOutputs availUtxos
             <> plutusV2OtherScript swapVal
      tx' =
        -- | Must spend all utxos to swap
        (foldl' 
            (\a (d,v) -> a 
              <> mustSpendScriptOutputWithMatchingDatumAndValue 
                    swapValHash 
                    (== toDatum (convert2SwapDatum d)) 
                    (==v) 
                    swapRedeemer) 
            mempty 
            swapUtxos
        )
        -- | Must output change to script address
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith swappableAddress (fmap toDatum' d) Nothing v)
              mempty
              swapChange
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Created a live swap address"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createLiveSwapAddress' = endpoint @"create-live-swap-address" createLiveSwapAddress
    burnBeacons' = endpoint @"burn-beacons" burnBeacons
    closeLiveAddress' = endpoint @"close-live-address" closeLiveAddress
    updateSwaps' = endpoint @"update-swaps" updateSwaps
    swapAssets' = endpoint @"swap-assets" swapAssets
    choices = 
      [ createLiveSwapAddress'
      , burnBeacons'
      , closeLiveAddress'
      , updateSwaps'
      , swapAssets'
      ]