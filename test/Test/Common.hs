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

{-# OPTIONS_GHC -Wno-orphans #-}

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
import Plutus.Script.Utils.V2.Scripts as UScripts
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl',repeat)
import Prelude as Haskell (Semigroup (..), String, IO)
import Cardano.Api.Shelley (ProtocolParameters (..))
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)
import qualified Cardano.Api as C
import Cardano.Api hiding (TxOutDatum(..),TxOutDatumInline,TxOutDatumHash,Address,TxId,Value)
import Cardano.Node.Emulator.Params
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Constraints.ValidityInterval
import Ledger.Address
import Ledger.Credential

import CardanoSwaps

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum Nothing val

instance ToJSON SwapConfig
instance FromJSON SwapConfig

instance ToJSON SwapDatum
instance FromJSON SwapDatum

instance ToJSON SwapRedeemer
instance FromJSON SwapRedeemer

instance ToJSON BeaconRedeemer
instance FromJSON BeaconRedeemer

data TestScripts = TestScripts
  { spendingValidator :: Validator
  , spendingValidatorHash :: ValidatorHash
  , beaconPolicy :: MintingPolicy
  , beaconPolicyHash :: MintingPolicyHash
  , beaconCurrencySymbol :: CurrencySymbol
  } deriving (Generic,ToJSON,FromJSON)

-------------------------------------------------
-- Params
-------------------------------------------------
data OpenSwapAddressParams = OpenSwapAddressParams
  { openSwapAddressBeaconsMinted :: [(TokenName,Integer)]
  , openSwapAddressBeaconRedeemer :: BeaconRedeemer
  , openSwapAddressAddress :: Address
  , openSwapAddressInfo :: [(Maybe SwapDatum, Value)]
  , openSwapAddressAsInline :: Bool
  , openSwapAddressScripts :: TestScripts
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "open-swap-address" OpenSwapAddressParams

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: C.Value
    user1 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000

    user2 :: C.Value
    user2 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
    
    user3 :: C.Value
    user3 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
    
    user4 :: C.Value
    user4 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
    
    user5 :: C.Value
    user5 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000

    user6 :: C.Value
    user6 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
  
    wallets :: [(Wallet,C.Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      ]

-------------------------------------------------
-- Trace Models
-------------------------------------------------
openSwapAddress :: OpenSwapAddressParams -> Contract () TraceSchema Text ()
openSwapAddress OpenSwapAddressParams{openSwapAddressScripts=TestScripts{..},..} = do
  let beaconRedeemer = toRedeemer openSwapAddressBeaconRedeemer

      toDatum'
        | openSwapAddressAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = plutusV2MintingPolicy beaconPolicy
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          openSwapAddressBeaconsMinted
        )
        -- | Add assets
        <> (foldl'
                (\acc (d,v) -> acc <> mustPayToAddressWith openSwapAddressAddress (fmap toDatum' d) v)
                mempty
                openSwapAddressInfo
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swap address created"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    openSwapUtxo' = endpoint @"open-swap-address" openSwapAddress
    choices = 
      [ openSwapUtxo'
      ]