{-# OPTIONS_GHC -Wno-orphans #-}

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
{-# LANGUAGE FlexibleInstances #-}

module Test.Internal
  (
    -- * Helper Types
    TokenMint(..)
  , UtxoInput(..)
  , UtxoOutput(..)
  
    -- * Model Parameters
  , CreateReferenceScriptParams(..)
  , CreateTransactionParams(..)

    -- * Endpoints
  , endpoints

    -- * Helper Functions
  , txOutRefWithValue
  , txOutRefWithValueAndDatum
  , txOutRefsAndDatumsAtAddress
  , toRedeemer
  , toDatum
  , unValidatorHash
  , unMintingPolicyHash
  , grouped
  , (.*.)
  , (.+.)
  , (.-.)
  , ceiling

    -- * Re-exports
  , module Data.Default
  , module Plutus.V2.Ledger.Api
  , module Ledger.Tx.Constraints.ValidityInterval
  , module Plutus.V1.Ledger.Value
  , module Plutus.Script.Utils.Ada
  , module Plutus.Script.Utils.V2.Generators
  , module Cardano.Node.Emulator.Params
  , ProtocolParameters(..)
  , EmulatorTrace
  , EmulatorConfig(..)
  , params
  , Address(..)
  , (.~)
  , (&)
  , PaymentPubKeyHash(..)
  , TxOutDatum(..)
  , TxOutRef(..)
  , void
  , zipWithM_
  , liftM2
  ) where

import qualified Data.Map as Map
import Control.Lens hiding (from,index,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void,zipWithM_,liftM2)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding 
  (txOutDatum,value,singleton,mintingPolicyHash,Value,lovelaceValueOf,from,validatorHash)
import Ledger.Tx.Constraints as Constraints
import qualified Ledger.Tx.Constraints.TxConstraints as Constraints
import Ledger.Tx.Constraints.TxConstraints (TxOutDatum(..),mustMintCurrencyWithRedeemerAndReference)
import Plutus.Contract hiding (waitNSlots)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..))
import Plutus.Script.Utils.Value
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Plutus.Trace
import qualified Prelude as Haskell
import Prelude (Semigroup (..))
import qualified Cardano.Api.Shelley as Api
import Cardano.Api.Shelley (ProtocolParameters (..)) 
import Cardano.Api hiding (TxOutDatum(..),TxOutDatumInline,TxOutDatumHash,Address,TxId,Value)
import Cardano.Node.Emulator.Params
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Scripts
import Ledger.Tx.Constraints.ValidityInterval
import Data.Foldable (foldMap',foldl')
import Plutus.Script.Utils.V2.Generators
import Plutus.V2.Ledger.Api (Credential(..), StakingCredential(..), FromData(..))
import Plutus.V1.Ledger.Value

import CardanoSwaps.Utils

-------------------------------------------------
-- Helper Types
-------------------------------------------------
-- | Mint or burn native tokens. Can use either a reference script or a local script.
data TokenMint = TokenMint
  { -- | The minting policy that must witness the transaction. The `Maybe` tuple is the reference script
    -- information (Location,TxOutRef) if the script should be used as a reference script. Pass in `Nothing`
    -- if the script should be used locally.
    mintWitness :: (MintingPolicy, Maybe (Address, TxOutRef))
  , mintRedeemer :: Redeemer
  , mintTokens :: [(TokenName, Integer)] -- ^ A list of tokens to mint/burn with this policy.
  } deriving (Generic,ToJSON,FromJSON)

-- | Consume UTxOs from a specific address. Can use either a reference script or a local script.
-- When a PubKey input is consumed, the required signature is not automatically included.
data UtxoInput 
  = ScriptUtxoInput
    { -- | The validator is the script that must witness the transaction. The `Maybe` tuple is the reference
      -- script information (Location,TxOutRef) if the script should be used as a referenec script. Pass in
      -- `Nothing` if the script should be used locally.
      spendWitness :: (Validator, Maybe (Address, TxOutRef)) 
    , spendRedeemer :: Redeemer
    , spendFromAddress :: Address
    , spendUtxos :: [TxOutRef] -- ^ A list of all UTxOs to be spent.
    }
  | PubKeyUtxoInput
    { pubKeyAddress :: Address
    , pubKeyUtxos :: [TxOutRef]
    } 
  deriving (Generic,ToJSON,FromJSON)

-- | Create a transaction output at the specified address with the specified value and datum.
data UtxoOutput = UtxoOutput
  { toAddress :: Address
  , outputUtxos :: [(Maybe (TxOutDatum Datum), Value)]
  } deriving (Generic,ToJSON,FromJSON)

-------------------------------------------------
-- Params
-------------------------------------------------
-- | Used to create a reference script UTxO at the specified address with the specified value and datum.
data CreateReferenceScriptParams = CreateReferenceScriptParams
  { createReferenceScriptScript :: Ledger.Script
  , createReferenceScriptAddress :: Address
  , createReferenceScriptUTxO :: (Value,TxOutDatum Datum) 
  } deriving (Generic,ToJSON,FromJSON)

-- | Used to create a transaction with the specified constraints.
data CreateTransactionParams = CreateTransactionParams
  { tokens :: [TokenMint]
  , inputs :: [UtxoInput]
  , outputs :: [UtxoOutput]
  , validityRange :: ValidityInterval POSIXTime
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-reference-script" CreateReferenceScriptParams
  .\/ Endpoint "create-transaction" CreateTransactionParams

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createReferenceScript :: CreateReferenceScriptParams -> Contract () TraceSchema Text ()
createReferenceScript (CreateReferenceScriptParams{createReferenceScriptUTxO = (value,datum),..}) = do
    ledgerTx <- submitTxConstraintsWith @Void lookups tx'
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Text "Reference UTxO created"
  where
    refDatum = Just datum
    validator = Validator $ createReferenceScriptScript
    refScript = unValidatorHash $ validatorHash validator
    lookups = plutusV2OtherScript validator
    tx' = 
      mustPayToAddressWith
        createReferenceScriptAddress 
        refDatum
        (Just refScript)
        value 
    
createTransaction :: CreateTransactionParams -> Contract () TraceSchema Text ()
createTransaction CreateTransactionParams{..} = do
    userPubKeyHash <- ownFirstPaymentPubKeyHash
    lookups <- genLookups
    ledgerTx <- submitTxConstraintsWith @Void lookups $ constraints userPubKeyHash
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Text "Tx Submitted"
  where
    addrLookup = fmap Constraints.unspentOutputs 
               . utxosAt 
               . unsafeFromRight 
               . toCardanoAddressInEra Mainnet

    mintLookup (TokenMint (mp,Nothing) _ _) = return $ plutusV2MintingPolicy mp
    mintLookup (TokenMint (_,Just(addr,_)) _ _) = addrLookup addr

    inputLookup (ScriptUtxoInput (val,Nothing) _ inputAddr _) = 
      (plutusV2OtherScript val <>) <$> addrLookup inputAddr
    inputLookup (ScriptUtxoInput (_,Just(refAddr,_)) _ inputAddr _) = 
      (<>) <$> addrLookup refAddr <*> addrLookup inputAddr
    inputLookup (PubKeyUtxoInput pubAddr _) = addrLookup pubAddr

    genLookups = do
      -- Create the required lookups for the minting/burning.
      mintLookups <- mapM mintLookup tokens
      -- Create the required lookups for the spending.
      inputLookups <- mapM inputLookup inputs
      -- Combine them into a single `ScriptLookup`.
      return $ foldl' (<>) (plutusV2MintingPolicy alwaysSucceedPolicy)
             $ mintLookups <> inputLookups

    constraints user = mconcat
      [ -- Must be signed by user.
        mustBeSignedBy user

        -- Must specify validity range.
      , mustValidateInTimeRange validityRange

        -- Must mint/burn tokens.
      , foldMap' executePolicy tokens
        
        -- Must consume UTxOs.
      , foldMap' consumeInput inputs

        -- Create new UTxOs.
      , foldMap' createUTxO outputs
      ]

-------------------------------------------------
-- Endpoints
-------------------------------------------------
-- | Endpoints for creating reference scripts and a generic transaction.
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createReferenceScript' = endpoint @"create-reference-script" createReferenceScript
    createTransaction' = endpoint @"create-transaction" createTransaction
    choices = 
      [ createReferenceScript'
      , createTransaction'
      ]

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Find the TxOutRef for the first UTxO with a specific value.
txOutRefWithValue :: Value -> EmulatorTrace TxOutRef
txOutRefWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((ref,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = ref
        | otherwise = findTxId v ys
      findTxId _ _ = Haskell.error "Test.Common.txOutRefWithValue error"
  return $ findTxId value' xs

-- | Find the TxOutRef for the first UTxO with a specifc value and datum.
txOutRefWithValueAndDatum :: PlutusTx.ToData a => Value -> a -> EmulatorTrace TxOutRef
txOutRefWithValueAndDatum value' datum = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      dHash = datumHash $ toDatum datum
      findTxId v dh ((ref,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = 
            case I.txOutDatumHash o of
              Just d' -> if d' == dh then ref else findTxId v dh ys
              Nothing -> findTxId v dh ys
        | otherwise = findTxId v dh ys
      findTxId _ _ _ = Haskell.error "Test.Common.txOutRefWithValueAndDatum error"
  return $ findTxId value' dHash xs

-- | Find all TxOutRefs and their datums located at the address.
txOutRefsAndDatumsAtAddress :: (FromData a) => Address -> EmulatorTrace [(TxOutRef,Maybe a)]
txOutRefsAndDatumsAtAddress addr = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId _ [] = []
      findTxId addr' ((ref,o):ys)
        | addr' == (toPlutusAddress $ I.txOutAddress o) = (ref,txOutDatum o) : findTxId addr' ys
        | otherwise = findTxId addr' ys
  return $ findTxId addr xs

toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

unValidatorHash :: ValidatorHash -> Ledger.ScriptHash
unValidatorHash (ValidatorHash h) = ScriptHash h

unMintingPolicyHash :: MintingPolicyHash -> Ledger.ScriptHash
unMintingPolicyHash (MintingPolicyHash h) = ScriptHash h

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Maybe (Ledger.ScriptHash) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum maybeScript val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum maybeScript val

executePolicy :: TokenMint -> TxConstraints i o
executePolicy (TokenMint (pol,Nothing) r ts) =
  -- Mint/burn each token with the minting policy.
  foldMap'
    (\(t,i) -> mustMintCurrencyWithRedeemerAndReference
      Nothing
      (mintingPolicyHash pol)
      r
      t
      i
    )
    ts
executePolicy (TokenMint (pol,Just (_,ref)) r ts) = mconcat
  [ -- Mint/burn each token with the minting policy.
    foldMap' 
      (\(t,i) -> mustMintCurrencyWithRedeemerAndReference 
        (Just ref)
        (mintingPolicyHash pol)
        r
        t
        i
      )
      ts
    
    -- Reference script.
  , mustReferenceOutput ref
  ]

consumeInput :: UtxoInput -> TxConstraints i o
consumeInput (ScriptUtxoInput (pol,Nothing) r _ is) = 
  -- Consume each UTxO from address.
  foldMap' (Constraints.singleton . \i -> MustSpendScriptOutput i r Nothing) is
consumeInput (ScriptUtxoInput (pol,Just (_,ref)) r _ is) = mconcat
  [ -- Consume each UTxO from address. 
    foldMap' (Constraints.singleton . \i -> MustSpendScriptOutput i r (Just ref)) is
    
    -- Reference script.
  , mustReferenceOutput ref
  ]
consumeInput (PubKeyUtxoInput _ refs) =
  -- Consume each UTxO from address.
  foldMap' (Constraints.singleton . \i -> MustSpendPubKeyOutput i) refs

createUTxO :: UtxoOutput -> TxConstraints i o
createUTxO (UtxoOutput addr utxos) = 
  foldMap' (\(d,v) -> mustPayToAddressWith addr d Nothing v) utxos

grouped :: Haskell.Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = 
  let (m,ms) = Haskell.splitAt n xs 
  in m : grouped n ms

txOutDatum :: forall d . FromData d => I.TxOut -> Maybe d
txOutDatum (I.TxOut (Api.TxOut _aie _tov tod _rs)) =
  case tod of
    Api.TxOutDatumNone ->
      Nothing
    Api.TxOutDatumHash _era _scriptDataHash ->
      Nothing
    Api.TxOutDatumInline _era scriptData ->
      fromBuiltinData @d  $ PlutusTx.dataToBuiltinData $ Api.toPlutusData scriptData
    Api.TxOutDatumInTx _era scriptData ->
      fromBuiltinData @d  $ PlutusTx.dataToBuiltinData $ Api.toPlutusData scriptData

(.*.) :: Rational -> Rational -> Rational
r1 .*. r2 = r1 * r2

(.+.) :: Rational -> Rational -> Rational
r1 .+. r2 = r1 + r2

(.-.) :: Rational -> Rational -> Rational
r1 .-. r2 = r1 - r2

ceiling :: Rational -> Integer
ceiling r
  | fromInteger rounded >= r = rounded
  | otherwise = rounded + 1
  where
    rounded = round r
