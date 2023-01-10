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

import Prelude as Haskell (Semigroup (..), Show, foldMap,String,IO)

import CardanoSwaps

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

createSwap :: AsContractError e => SwapParams -> Contract w s e ()
createSwap SwapParams{..} = do
  let swapConfig = SwapConfig
        { swapOwner = swapParamsOwner
        , swapOffer = swapParamsOffer
        , swapAsk = swapParamsAsk
        }
      swap = swapTypedValidator beaconSymbol swapConfig
      swapHash = Scripts.validatorHash swap
      beaconPolicyHash = mintingPolicyHash $ beaconPolicy beaconVaultValidatorHash
      beaconVal = singleton beaconSymbol "TestBeacon" beaconMint
      beaconMintRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData $ (MintBeacon "TestBeacon")
      beaconVaultDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData beaconSymbol
      refDeposit = lovelaceValueOf refScriptDeposit <> beaconVal
      beaconVaultAddress = scriptValidatorHashAddress beaconVaultValidatorHash Nothing
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
        <> mustPayToTheScriptWithInlineDatum initialPrice ((uncurry singleton $ swapOffer swapConfig) initialPosition)
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "opened a swap"

endpoints :: Contract () SwapSchema Text ()
endpoints = awaitPromise (endpoint @"create-swap" createSwap) >> endpoints

swapTrace :: EmulatorTrace ()
swapTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $ 
    SwapParams
      { swapParamsOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , swapParamsOffer = (adaSymbol,adaToken)
      , swapParamsAsk = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken")
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , beaconDeposit = 2_000_000
      , beaconMint = 1
      , refScriptDeposit = 28_000_000
      }

  void $ waitUntilSlot 2

test :: IO ()
test = runEmulatorTraceIO swapTrace