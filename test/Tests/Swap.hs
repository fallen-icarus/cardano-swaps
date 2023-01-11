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

module Tests.Swap
(
  test
) where

import Data.Void (Void)
import Control.Lens
import qualified Data.Map as Map
import Data.Default
import Text.Printf
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import Plutus.V1.Ledger.Contexts as V
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty

import Prelude as Haskell (Semigroup (..), Show, foldMap,String,IO)

import CardanoSwaps

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

-------------------------------------------------
-- Helper Constraints
-------------------------------------------------
mustPayToScriptWithInlineDatumAndRefScript :: o -> ValidatorHash -> Value -> TxConstraints i o
mustPayToScriptWithInlineDatumAndRefScript dt (ValidatorHash h) val =
  mempty { txOwnOutputs = [ScriptOutputConstraint (TxOutDatumInline dt) val (Just $ ScriptHash h)]}

data SwapParams = SwapParams
  { swapParamsOwner :: PaymentPubKeyHash
  , swapParamsOffer :: (CurrencySymbol,TokenName)
  , swapParamsAsk :: (CurrencySymbol,TokenName)
  , initialPrice :: Price
  , initialPosition :: Integer
  , beaconDeposit :: Integer  -- ^ In lovelace
  , beaconMint :: Integer  -- ^ Positive or negative
  , refScriptDeposit :: Integer  -- ^ In lovelace
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

type SwapSchema =
      Endpoint "create-swap" SwapParams
  .\/ Endpoint "add-swap-position" ()
  .\/ Endpoint "close-swap" ()
  .\/ Endpoint "update-swap-prices" ()
  .\/ Endpoint "swap" ()

mkSchemaDefinitions ''SwapSchema

data Swap
instance Scripts.ValidatorTypes Swap where
  type instance RedeemerType Swap = Action
  type instance DatumType Swap = Price

createSwap :: SwapParams -> Contract () s Text ()
createSwap SwapParams{..} = do
  let swapConfig = SwapConfig
        { swapOwner = swapParamsOwner
        , swapOffer = swapParamsOffer
        , swapAsk = swapParamsAsk
        }
      swap = swapTypedValidator beaconSymbol swapConfig
      swapHash = Scripts.validatorHash swap
      swapAddress = scriptValidatorHashAddress swapHash Nothing
      beaconPolicyHash = mintingPolicyHash $ beaconPolicy beaconVaultValidatorHash
      beaconVal = singleton beaconSymbol "TestBeacon" beaconMint
      beaconMintRedeemer = toRedeemer (MintBeacon "TestBeacon")
      beaconVaultDatum = toDatum beaconSymbol
      refDeposit = lovelaceValueOf refScriptDeposit <> beaconVal
      beaconVaultAddress = scriptValidatorHashAddress beaconVaultValidatorHash Nothing
      priceDatum = toDatum initialPrice
      lookups = plutusV2OtherScript beaconVault
             <> plutusV2MintingPolicy (beaconPolicy beaconVaultValidatorHash)
             <> typedValidatorLookups swap
      tx = 
        -- | Mint beacon
        mustMintCurrencyWithRedeemer beaconPolicyHash beaconMintRedeemer "TestBeacon" beaconMint
        -- | Send deposit amount to beacon vault
        <> mustPayToOtherScriptWithInlineDatum beaconVaultValidatorHash beaconVaultDatum (lovelaceValueOf beaconDeposit)
        -- | Store reference script in swap address with deposit and beacon
        <> mustPayToScriptWithInlineDatumAndRefScript initialPrice swapHash refDeposit
        -- | Add first position
        <> mustPayToAddressWithInlineDatum swapAddress priceDatum ((uncurry singleton $ swapOffer swapConfig) initialPosition)
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  
  logInfo @String "opened a swap"

endpoints :: Contract () SwapSchema Text ()
endpoints = awaitPromise (endpoint @"create-swap" createSwap) >> endpoints

-- | A trace where the beacon deposit is deliberately too small.
-- This should produce a failed transaction when createSwap is called.
beaconDepositTooSmallTrace :: EmulatorTrace ()
beaconDepositTooSmallTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    SwapParams
      { swapParamsOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , swapParamsOffer = (adaSymbol,adaToken)
      , swapParamsAsk = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , beaconDeposit = 1_000_000
      , beaconMint = 1
      , refScriptDeposit = 28_000_000
      }

  void $ waitUntilSlot 2

beaconDepositTooLargeTrace :: EmulatorTrace ()
beaconDepositTooLargeTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    SwapParams
      { swapParamsOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , swapParamsOffer = (adaSymbol,adaToken)
      , swapParamsAsk = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , beaconDeposit = 3_000_000
      , beaconMint = 1
      , refScriptDeposit = 28_000_000
      }

  void $ waitUntilSlot 2

successfullCreateSwapTrace :: EmulatorTrace ()
successfullCreateSwapTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    SwapParams
      { swapParamsOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , swapParamsOffer = (adaSymbol,adaToken)
      , swapParamsAsk = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , beaconDeposit = 2_000_000
      , beaconMint = 1
      , refScriptDeposit = 28_000_000
      }

  void $ waitUntilSlot 2

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    currSym :: CurrencySymbol
    currSym = "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"

    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000 <> singleton currSym "TestToken1" 100

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000 <> singleton currSym "TestToken1" 100

    user3 :: Value
    user3 = lovelaceValueOf 1_000_000_000 <> singleton currSym "TestToken1" 100 

    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      ]

test :: TestTree
test = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create Swap"
    [ checkPredicateOptions opts "Beacon Deposit Too Small" 
        (Test.not assertNoFailedTransactions) beaconDepositTooSmallTrace
    , checkPredicateOptions opts "Beacon Deposit Too Large" 
        (Test.not assertNoFailedTransactions) beaconDepositTooLargeTrace
    , checkPredicateOptions opts "Successfull Create Swap" 
        assertNoFailedTransactions successfullCreateSwapTrace
    ]