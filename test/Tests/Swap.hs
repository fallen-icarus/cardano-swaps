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

import Control.Lens
import qualified Data.Map as Map
import Data.Default
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Playground.Contract (ToSchema)
import Playground.TH (mkSchemaDefinitions)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.List (foldl')

import Prelude as Haskell (Semigroup (..), String)

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

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000 
         <> (uncurry singleton testToken1) 100

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000 
         <> (uncurry singleton testToken1) 100

    user3 :: Value
    user3 = lovelaceValueOf 1_000_000_000 
         <> (uncurry singleton testToken1) 100 

    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      ]

data CreateSwapParams = CreateSwapParams
  { createSwapOwner :: PaymentPubKeyHash
  , createSwapOffer :: (CurrencySymbol,TokenName)
  , createSwapAsk :: (CurrencySymbol,TokenName)
  , initialPrice :: Price
  , initialPosition :: Integer
  , createbeaconDeposit :: Integer  -- ^ In lovelace
  , createBeaconMint :: Integer  -- ^ Only positive is relevant, negative would be unbalanced
  , refScriptDeposit :: Integer  -- ^ In lovelace
  , beaconStoredWithRefScript :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data UpdatePricesParams = UpdatePricesParams
  { updateSwapOwner :: PaymentPubKeyHash
  , updateSwapOffer :: (CurrencySymbol,TokenName)
  , updateSwapAsk :: (CurrencySymbol,TokenName)
  , newPrice :: Price
  , asInline :: Bool
  , newPosition :: Integer
  , utxosToUpdate :: [(Price,Value)]
  , extraRecipients :: [(Address,Value)]  -- ^ Aside from swap address and owner
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data CloseSwapParams = CloseSwapParams
  { closeSwapOwner :: PaymentPubKeyHash
  , closeSwapOffer :: (CurrencySymbol,TokenName)
  , closeSwapAsk :: (CurrencySymbol,TokenName)
  , burnBeacon :: Bool
  , closeBeaconDeposit :: Integer
  , closeBeaconBurn :: Integer
  , utxosToClose :: [(Price,Value)]
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data ExecSwapParams = ExecSwapParams
  { execSwapSwapOwner :: PaymentPubKeyHash
  , execSwapSwapOffer :: (CurrencySymbol,TokenName)
  , execSwapSwapAsk :: (CurrencySymbol,TokenName)
  , valueToGive :: Value
  , utxosToSwap :: [(Price,Value)]
  , weightedAvg :: Price
  , swapChange :: Value
  , datumAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

type SwapSchema =
      Endpoint "create-swap" CreateSwapParams
  .\/ Endpoint "close-swap" CloseSwapParams
  .\/ Endpoint "update-swap-prices" UpdatePricesParams
  .\/ Endpoint "swap" ExecSwapParams

mkSchemaDefinitions ''SwapSchema

data Swap
instance Scripts.ValidatorTypes Swap where
  type instance RedeemerType Swap = Action
  type instance DatumType Swap = Price

createSwap :: CreateSwapParams -> Contract () SwapSchema Text ()
createSwap CreateSwapParams{..} = do
  let swapConfig = SwapConfig
        { swapOwner = createSwapOwner
        , swapOffer = createSwapOffer
        , swapAsk = createSwapAsk
        }
      swap = swapTypedValidator beaconSymbol swapConfig
      swapHash = Scripts.validatorHash swap
      beaconPolicyHash = mintingPolicyHash $ beaconPolicy beaconVaultValidatorHash
      beaconVal = singleton beaconSymbol "TestBeacon" createBeaconMint
      beaconMintRedeemer = toRedeemer $ (MintBeacon "TestBeacon")
      beaconVaultDatum = toDatum beaconSymbol
      initialPosVal = uncurry singleton (swapOffer swapConfig) $ initialPosition
      (refDeposit,pos) = 
        if beaconStoredWithRefScript
        then (lovelaceValueOf refScriptDeposit <> beaconVal, lovelaceValueOf initialPosition)
        else (lovelaceValueOf refScriptDeposit, initialPosVal <> beaconVal)
      lookups = plutusV2OtherScript beaconVault
             <> plutusV2MintingPolicy (beaconPolicy beaconVaultValidatorHash)
             <> typedValidatorLookups swap
      tx' = 
        -- | Mint beacon
        mustMintCurrencyWithRedeemer beaconPolicyHash beaconMintRedeemer "TestBeacon" createBeaconMint
        -- | Send deposit amount to beacon vault
        <> mustPayToOtherScriptWithInlineDatum beaconVaultValidatorHash beaconVaultDatum (lovelaceValueOf createbeaconDeposit)
        -- | Store reference script in swap address with deposit and beacon
        <> mustPayToScriptWithInlineDatumAndRefScript initialPrice swapHash refDeposit
        -- | Add first position
        <> mustPayToTheScriptWithInlineDatum initialPrice pos
  ledgerTx <- submitTxConstraintsWith lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "opened a swap"

updatePrices :: UpdatePricesParams -> Contract () SwapSchema Text ()
updatePrices UpdatePricesParams{..} = do
  let swapConfig = SwapConfig
        { swapOwner = updateSwapOwner
        , swapOffer = updateSwapOffer
        , swapAsk = updateSwapAsk
        }

      -- | Swap Contract
      swap = swapTypedValidator beaconSymbol swapConfig
      swapHash = Scripts.validatorHash swap
      swapAddress = scriptValidatorHashAddress swapHash Nothing

      -- | Datums, Redeemers, and Values
      updateRedeemer = toRedeemer $ UpdatePrices newPrice
      newVal = uncurry singleton (swapOffer swapConfig) $ newPosition
  
  utxos <- utxosAt swapAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  
  let lookups = typedValidatorLookups swap
             <> Constraints.unspentOutputs utxos
      tx' =
        -- | Must be signed by owner
        mustBeSignedBy userPubKeyHash
        -- | Must spend all utxos to be updated
        <> foldl' (\a (price,val) -> a 
                     <> mustSpendScriptOutputWithMatchingDatumAndValue 
                          swapHash (== toDatum price) (==val) updateRedeemer) 
               mempty 
               utxosToUpdate
        -- | Must recreate desired position at swap address
        <> (if asInline
           then mustPayToTheScriptWithInlineDatum newPrice newVal
           else mustPayToTheScriptWithDatumHash newPrice newVal)
        -- | Must pay to extra addresses aside from swap address
        <> (if null extraRecipients
           then mempty
           else foldl' (\a (addr,v) -> a <> mustPayToAddress addr v) mempty extraRecipients)

  ledgerTx <- submitTxConstraintsWith lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  
  logInfo @String "updated swap prices"

closeSwap :: CloseSwapParams -> Contract () SwapSchema Text ()
closeSwap CloseSwapParams{..} = do
  let swapConfig = SwapConfig
        { swapOwner = closeSwapOwner
        , swapOffer = closeSwapOffer
        , swapAsk = closeSwapAsk
        }
      swap = swapTypedValidator beaconSymbol swapConfig
      swapHash = Scripts.validatorHash swap
      swapAddress = scriptValidatorHashAddress swapHash Nothing
      beaconPolicyHash = mintingPolicyHash $ beaconPolicy beaconVaultValidatorHash
      beaconBurnRedeemer = toRedeemer $ (BurnBeacon "TestBeacon")
      beaconVaultDatum = toDatum beaconSymbol
      beaconVaultAddress = scriptValidatorHashAddress beaconVaultValidatorHash Nothing
      closeRedeemer = toRedeemer Close

  swapUtxos <- utxosAt swapAddress
  beaconVaultUtxos <- utxosAt beaconVaultAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let lookups = typedValidatorLookups swap
             <> plutusV2OtherScript beaconVault
             <> plutusV2MintingPolicy (beaconPolicy beaconVaultValidatorHash)
             <> Constraints.unspentOutputs swapUtxos
             <> Constraints.unspentOutputs beaconVaultUtxos
             <> Constraints.otherData beaconVaultDatum
      
      tx' =
        (if burnBeacon
        then -- | Burn beacon
             mustMintCurrencyWithRedeemer beaconPolicyHash beaconBurnRedeemer "TestBeacon" closeBeaconBurn
             -- | Withdrawal deposit
             <> mustSpendScriptOutputWithMatchingDatumAndValue beaconVaultValidatorHash (==beaconVaultDatum) (==lovelaceValueOf closeBeaconDeposit) beaconBurnRedeemer
        else mempty)
        -- | Must spend all utxos to be closed
        <> (foldl' (\a (price,val) -> a 
                     <> mustSpendScriptOutputWithMatchingDatumAndValue 
                          swapHash (== toDatum price) (==val) closeRedeemer) 
               mempty 
               utxosToClose)
        -- | Must be signed by owner
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  
  logInfo @String "swap closed"

swapAssets :: ExecSwapParams -> Contract () SwapSchema Text ()
swapAssets ExecSwapParams{..} = do
  let swapConfig = SwapConfig
        { swapOwner = execSwapSwapOwner
        , swapOffer = execSwapSwapOffer
        , swapAsk = execSwapSwapAsk
        }

      -- | Swap Contract
      swap = swapTypedValidator beaconSymbol swapConfig
      swapHash = Scripts.validatorHash swap
      swapAddress = scriptValidatorHashAddress swapHash Nothing

      -- | Datums, Redeemers, and Values
      swapRedeemer = toRedeemer Swap
      newSwapVal = valueToGive <> swapChange

      
  utxos <- utxosAt swapAddress

  let lookups = typedValidatorLookups swap
             <> Constraints.unspentOutputs utxos
      tx' = 
        -- | Must spend all utxos to be swapped
        foldl' (\a (price,val) -> a 
                     <> mustSpendScriptOutputWithMatchingDatumAndValue 
                          swapHash (== toDatum price) (==val) swapRedeemer) 
               mempty 
               utxosToSwap
        -- | Must return change to swap address
        <> (if datumAsInline
            then mustPayToTheScriptWithInlineDatum weightedAvg newSwapVal
            else mustPayToTheScriptWithDatumHash weightedAvg newSwapVal)

  ledgerTx <- submitTxConstraintsWith lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  
  logInfo @String "swapped assets"

endpoints :: Contract () SwapSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createSwap' = endpoint @"create-swap" createSwap
    updatePrices' = endpoint @"update-swap-prices" updatePrices
    closeSwap' = endpoint @"close-swap" closeSwap
    swapAssets' = endpoint @"swap" swapAssets
    choices = 
      [ createSwap'
      , updatePrices'
      , closeSwap'
      , swapAssets'
      ]

-- | A trace where the beacon deposit is deliberately too small.
-- This should produce a failed transaction when createSwap is called.
beaconDepositTooSmallTrace :: EmulatorTrace ()
beaconDepositTooSmallTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 1_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }

  void $ waitUntilSlot 2

-- | A trace where the beacon deposit is deliberately too large.
-- This should produce a failed transaction when createSwap is called.
beaconDepositTooLargeTrace :: EmulatorTrace ()
beaconDepositTooLargeTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 3_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }

  void $ waitUntilSlot 2

-- | A trace where too many beacons are minted.
-- This should produce a failed transaction when createSwap is called.
tooManyBeaconsMintedTrace :: EmulatorTrace ()
tooManyBeaconsMintedTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 2
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }

  void $ waitUntilSlot 2

-- | A trace where there is no beacon deposit.
-- This should produce a failed transaction when createSwap is called.
noBeaconDepositTrace :: EmulatorTrace ()
noBeaconDepositTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 0_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }

  void $ waitUntilSlot 2

-- | A trace where everything is correct.
-- This should produce a successfull transaction when createSwap is called.
successfullCreateSwapTrace :: EmulatorTrace ()
successfullCreateSwapTrace = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }

  void $ waitUntilSlot 2

-- | A trace where everything is correct.
-- This should produce a successfull transaction when updatePrices is called.
successfullPriceUpdates :: EmulatorTrace ()
successfullPriceUpdates = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"update-swap-prices" h1 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio 1 1
      , asInline = True
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [(unsafeRatio 3 2,lovelaceValueOf 10_000_000)]
      , extraRecipients = [] 
      }

  void $ waitUntilSlot 4

-- | A trace where the reference script datum is updated.
-- This should produce a failed transaction when updatePrices is called.
updateRefPrice :: EmulatorTrace ()
updateRefPrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  let beaconVal = singleton beaconSymbol "TestBeacon" 1
  callEndpoint @"update-swap-prices" h1 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio 1 1
      , asInline = True
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [ (unsafeRatio 3 2,lovelaceValueOf 10_000_000)
          , (unsafeRatio 3 2,lovelaceValueOf 28_000_000 <> beaconVal)
          ]
      , extraRecipients = [] 
      }

  void $ waitUntilSlot 4

-- | A trace where a nonowner tries to update prices.
-- This should produce a failed transaction when updatePrices is called.
nonOwnerUpdatesPrice :: EmulatorTrace ()
nonOwnerUpdatesPrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"update-swap-prices" h2 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio 1 1
      , asInline = True
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [(unsafeRatio 3 2,lovelaceValueOf 10_000_000)]
      , extraRecipients = [] 
      }

  void $ waitUntilSlot 4

-- | A trace where the beacon token is withdrawn.
-- This should produce a failed transaction when updatePrices is called.
removesBeaconToken :: EmulatorTrace ()
removesBeaconToken = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = False
      }
  
  void $ waitUntilSlot 2

  let beaconVal = singleton beaconSymbol "TestBeacon" 1
  callEndpoint @"update-swap-prices" h1 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio 1 1
      , asInline = True
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [(unsafeRatio 3 2,lovelaceValueOf 10_000_000 <> beaconVal)]
      , extraRecipients = [] 
      }

  void $ waitUntilSlot 4

-- | A trace where value is sent to other addresses.
-- This should produce a failed transaction when updatePrices is called.
valueSentToOtherAddresses :: EmulatorTrace ()
valueSentToOtherAddresses = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"update-swap-prices" h1 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio 1 1
      , asInline = True
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [(unsafeRatio 3 2,lovelaceValueOf 10_000_000)]
      , extraRecipients = 
          [(mockWalletAddress $ knownWallet 2, lovelaceValueOf 5_000_000)] 
      }

  void $ waitUntilSlot 4

-- | A trace where the new price is negative.
-- This should produce a failed transaction when updatePrices is called.
invalidNewPrice :: EmulatorTrace ()
invalidNewPrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"update-swap-prices" h1 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio (-2) 1
      , asInline = True
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [(unsafeRatio 3 2,lovelaceValueOf 10_000_000)]
      , extraRecipients = [] 
      }

  void $ waitUntilSlot 4

-- | A trace where new price not supplied as inline datum.
-- This should produce a failed transaction when updatePrices is called.
nonInlinePrice :: EmulatorTrace ()
nonInlinePrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"update-swap-prices" h1 $
    UpdatePricesParams
      { updateSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , updateSwapOffer = (adaSymbol,adaToken)
      , updateSwapAsk = testToken1
      , newPrice = unsafeRatio 1 1
      , asInline = False
      , newPosition = 10_000_000
      , utxosToUpdate = 
          [(unsafeRatio 3 2,lovelaceValueOf 10_000_000)]
      , extraRecipients = [] 
      }

  void $ waitUntilSlot 4

-- | A trace where reference script is removed without burning beacon.
-- This should produce a failed transaction when closeSwap is called.
refScriptRemovedWithoutBurning :: EmulatorTrace ()
refScriptRemovedWithoutBurning = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = False
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"close-swap" h1 $
    CloseSwapParams
      { closeSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , closeSwapOffer = (adaSymbol,adaToken)
      , closeSwapAsk = testToken1
      , burnBeacon = False
      , closeBeaconDeposit = 2_000_000
      , closeBeaconBurn = -1
      , utxosToClose = 
          [ (unsafeRatio 3 2,lovelaceValueOf 28_000_000) ]
      }

  void $ waitUntilSlot 4

-- | A trace where a non-owner tries closing the swap.
-- This should produce a failed transaction when closeSwap is called.
nonOwnerCloses :: EmulatorTrace ()
nonOwnerCloses = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"close-swap" h2 $
    CloseSwapParams
      { closeSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , closeSwapOffer = (adaSymbol,adaToken)
      , closeSwapAsk = testToken1
      , burnBeacon = False
      , closeBeaconDeposit = 2_000_000
      , closeBeaconBurn = -1
      , utxosToClose = 
          [ (unsafeRatio 3 2,lovelaceValueOf 10_000_000) ]
      }

  void $ waitUntilSlot 4

-- | A trace where the beacon is burned without removing the reference script.
-- This should produce a Successfull transaction when closeSwap is called.
beaconBurnedWithoutRemovingRefScript :: EmulatorTrace ()
beaconBurnedWithoutRemovingRefScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 3 2
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = False
      }
  
  void $ waitUntilSlot 2

  let beaconVal = singleton beaconSymbol "TestBeacon" 1
  callEndpoint @"close-swap" h1 $
    CloseSwapParams
      { closeSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , closeSwapOffer = (adaSymbol,adaToken)
      , closeSwapAsk = testToken1
      , burnBeacon = True
      , closeBeaconDeposit = 2_000_000
      , closeBeaconBurn = -1
      , utxosToClose = 
          [ (unsafeRatio 3 2,lovelaceValueOf 10_000_000 <> beaconVal) ]
      }

  void $ waitUntilSlot 4

-- | A trace where everything is correct.
-- This should produce a successfull transaction when swapAssets is called.
successfullSwap :: EmulatorTrace ()
successfullSwap = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 2 1
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    ExecSwapParams
      { execSwapSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , execSwapSwapOffer = (adaSymbol,adaToken)
      , execSwapSwapAsk = testToken1
      , valueToGive = (uncurry singleton testToken1) 10
      , utxosToSwap =
          [ (unsafeRatio 2 1, lovelaceValueOf 10_000_000) ]
      , weightedAvg = unsafeRatio 2 1
      , swapChange = lovelaceValueOf 5_000_000
      , datumAsInline = True
      }

  void $ waitUntilSlot 4

-- | A trace where the price is not an inline datum.
-- This should produce a failed transaction when swapAssets is called.
swapChangeDatumNotInline :: EmulatorTrace ()
swapChangeDatumNotInline = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 2 1
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    ExecSwapParams
      { execSwapSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , execSwapSwapOffer = (adaSymbol,adaToken)
      , execSwapSwapAsk = testToken1
      , valueToGive = (uncurry singleton testToken1) 10
      , utxosToSwap =
          [ (unsafeRatio 2 1, lovelaceValueOf 10_000_000) ]
      , weightedAvg = unsafeRatio 2 1
      , swapChange = lovelaceValueOf 5_000_000
      , datumAsInline = False
      }

  void $ waitUntilSlot 4

-- | A trace where change datum is not the weighted average price.
-- This should produce a failed transaction when swapAssets is called.
swapChangeDatumNotWeightedAvg :: EmulatorTrace ()
swapChangeDatumNotWeightedAvg = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 2 1
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    ExecSwapParams
      { execSwapSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , execSwapSwapOffer = (adaSymbol,adaToken)
      , execSwapSwapAsk = testToken1
      , valueToGive = (uncurry singleton testToken1) 10
      , utxosToSwap =
          [ (unsafeRatio 2 1, lovelaceValueOf 10_000_000) ]
      , weightedAvg = unsafeRatio 1 1
      , swapChange = lovelaceValueOf 5_000_000
      , datumAsInline = True
      }

  void $ waitUntilSlot 4

-- | A trace where swap ratio not met.
-- This should produce a failed transaction when swapAssets is called.
swapRatioNotMet :: EmulatorTrace ()
swapRatioNotMet = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 2 1
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = True
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    ExecSwapParams
      { execSwapSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , execSwapSwapOffer = (adaSymbol,adaToken)
      , execSwapSwapAsk = testToken1
      , valueToGive = (uncurry singleton testToken1) 5
      , utxosToSwap =
          [ (unsafeRatio 2 1, lovelaceValueOf 10_000_000) ]
      , weightedAvg = unsafeRatio 2 1
      , swapChange = lovelaceValueOf 5_000_000
      , datumAsInline = True
      }

  void $ waitUntilSlot 4

-- | A trace where a non-offered asset is withdrawn.
-- This should produce a failed transaction when swapAssets is called.
swapNonOfferedAsset :: EmulatorTrace ()
swapNonOfferedAsset = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 2 1
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = False
      }
  
  void $ waitUntilSlot 2

  let beaconVal = singleton beaconSymbol "TestBeacon" 1
  callEndpoint @"swap" h2 $
    ExecSwapParams
      { execSwapSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , execSwapSwapOffer = (adaSymbol,adaToken)
      , execSwapSwapAsk = testToken1
      , valueToGive = (uncurry singleton testToken1) 10
      , utxosToSwap =
          [ (unsafeRatio 2 1, lovelaceValueOf 10_000_000 <> beaconVal) ]
      , weightedAvg = unsafeRatio 2 1
      , swapChange = lovelaceValueOf 5_000_000
      , datumAsInline = True
      }

  void $ waitUntilSlot 4

-- | A trace where swap reference script is consumed.
-- This should produce a failed transaction when swapAssets is called.
swapRefScriptUtxo :: EmulatorTrace ()
swapRefScriptUtxo = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"create-swap" h1 $
    CreateSwapParams
      { createSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , createSwapOffer = (adaSymbol,adaToken)
      , createSwapAsk = testToken1
      , initialPrice = unsafeRatio 2 1
      , initialPosition = 10_000_000
      , createbeaconDeposit = 2_000_000
      , createBeaconMint = 1
      , refScriptDeposit = 28_000_000
      , beaconStoredWithRefScript = False
      }
  
  void $ waitUntilSlot 2

  callEndpoint @"swap" h2 $
    ExecSwapParams
      { execSwapSwapOwner = mockWalletPaymentPubKeyHash $ knownWallet 1
      , execSwapSwapOffer = (adaSymbol,adaToken)
      , execSwapSwapAsk = testToken1
      , valueToGive = (uncurry singleton testToken1) 10
      , utxosToSwap =
          [ (unsafeRatio 2 1, lovelaceValueOf 28_000_000) ]
      , weightedAvg = unsafeRatio 2 1
      , swapChange = lovelaceValueOf 23_000_000
      , datumAsInline = True
      }

  void $ waitUntilSlot 4

test :: TestTree
test = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Cardano-Swaps"
    [ testGroup "Create Swap"
      [ checkPredicateOptions opts "Beacon Deposit Too Small" 
          (Test.not assertNoFailedTransactions) beaconDepositTooSmallTrace
      , checkPredicateOptions opts "Beacon Deposit Too Large" 
          (Test.not assertNoFailedTransactions) beaconDepositTooLargeTrace
      , checkPredicateOptions opts "No Beacon Deposit"
          (Test.not assertNoFailedTransactions) noBeaconDepositTrace
      , checkPredicateOptions opts "Too Many Beacons Minted"
          (Test.not assertNoFailedTransactions) tooManyBeaconsMintedTrace
      , checkPredicateOptions opts "Successfull Create Swap" 
          assertNoFailedTransactions successfullCreateSwapTrace
      ]
    , testGroup "Update Prices"
      [ checkPredicateOptions opts "Successfull Price Updates"
          assertNoFailedTransactions successfullPriceUpdates
      , checkPredicateOptions opts "Reference script price updated"
          (Test.not assertNoFailedTransactions) updateRefPrice
      , checkPredicateOptions opts "Non-owner updates prices"
          (Test.not assertNoFailedTransactions) nonOwnerUpdatesPrice
      , checkPredicateOptions opts "Beacon is withdrawn from swap address"
          (Test.not assertNoFailedTransactions) removesBeaconToken
      , checkPredicateOptions opts "Value sent to other addresses"
          (Test.not assertNoFailedTransactions) valueSentToOtherAddresses
      , checkPredicateOptions opts "Invalid new price"
          (Test.not assertNoFailedTransactions) invalidNewPrice
      , checkPredicateOptions opts "New price not as inline datum"
          (Test.not assertNoFailedTransactions) nonInlinePrice
      ]
    , testGroup "Close Swap"
      [ checkPredicateOptions opts "Reference script removed without burning beacon"
          (Test.not assertNoFailedTransactions) refScriptRemovedWithoutBurning
      , checkPredicateOptions opts "Non-owner closes swap"
          (Test.not assertNoFailedTransactions) nonOwnerCloses
      , checkPredicateOptions opts "Beacon burned without removing reference script"
          assertNoFailedTransactions beaconBurnedWithoutRemovingRefScript
      ]
    , testGroup "Swap Assets"
      [ checkPredicateOptions opts "Successfull asset swap"
          assertNoFailedTransactions successfullSwap
      , checkPredicateOptions opts "Swap change datum is not inline"
          (Test.not assertNoFailedTransactions) swapChangeDatumNotInline
      , checkPredicateOptions opts "Swap change datum is not weighted avg price"
          (Test.not assertNoFailedTransactions) swapChangeDatumNotWeightedAvg
      , checkPredicateOptions opts "Swap ratio not met"
          (Test.not assertNoFailedTransactions) swapRatioNotMet
      , checkPredicateOptions opts "Non-offered asset swapped"
          (Test.not assertNoFailedTransactions) swapNonOfferedAsset
      , checkPredicateOptions opts "Swap reference script utxo spent"
          (Test.not assertNoFailedTransactions) swapRefScriptUtxo
      ]
    ]

  -- runEmulatorTraceIO' def emConfig successfullSwap