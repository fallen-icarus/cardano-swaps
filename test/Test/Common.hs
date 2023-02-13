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
  { swapPrice :: Price
  , swapBeacon :: Maybe CurrencySymbol
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

convert2SwapDatum :: SwapDatum' -> SwapDatum
convert2SwapDatum (SwapDatum' price beaconSym) = SwapDatum price beaconSym

data Action' = Close' | Update' | Swap'
  deriving (Generic,ToJSON,FromJSON,ToSchema)

convert2Action :: Action' -> Action
convert2Action Close' = Close
convert2Action Update' = Update
convert2Action Swap' = Swap

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
  {
    beaconsBurned :: [(TokenName,Integer)]
  , useBurnRedeemer :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

type TraceSchema =
      Endpoint "create-live-swap-address" CreateLiveSwapAddressParams
  .\/ Endpoint "burn-beacons" BurnBeaconParams

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

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createLiveSwapAddress' = endpoint @"create-live-swap-address" createLiveSwapAddress
    burnBeacons' = endpoint @"burn-beacons" burnBeacons
    -- spend' = endpoint @"spend" spend
    choices = 
      [ createLiveSwapAddress'
      , burnBeacons'
      -- , spend'
      ]