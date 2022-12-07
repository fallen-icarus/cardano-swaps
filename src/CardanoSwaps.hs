{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module CardanoSwaps
(
  swap,
  BasicInfo (..),
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
-- import qualified Prelude as Haskell

import           Cardano.Api hiding (Script,Value,TxOut)
import           Cardano.Api.Shelley   (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
-- import Plutus.V1.Ledger.Value (flattenValue)
-- import Ledger.Bytes (fromHex)
-- import Data.String (fromString)
import qualified Plutonomy
import Ledger.Value (valueOf)
import PlutusTx.Numeric as Num
import Plutus.V2.Ledger.Tx
-- import PlutusTx.Ratio (recip)
import Ledger.Ada (lovelaceValueOf)

-------------------------------------------------
-- Swap Settings
-------------------------------------------------
-- for use as extra parameter; creates a unique address
data BasicInfo = BasicInfo
  {
    owner :: PaymentPubKeyHash,
    offerAsset :: (CurrencySymbol,TokenName),
    askAsset :: (CurrencySymbol,TokenName)
  }

PlutusTx.makeLift ''BasicInfo

newtype Price = Price 
  -- offerAsset/askAsset
  { getPrice :: Rational }

instance Eq Price where
  {-# INLINABLE (==) #-}
  (Price x) == (Price y) = x == y

PlutusTx.unstableMakeIsData ''Price

data Action = Close
            | UpdatePrices Price
            | Swap

PlutusTx.unstableMakeIsData ''Action

-------------------------------------------------
-- On-Chain Swap
-------------------------------------------------
mkSwap :: BasicInfo -> Price -> Action -> ScriptContext -> Bool
mkSwap BasicInfo{..} price action ctx@ScriptContext{scriptContextTxInfo = info} = case action of
  Close ->
    -- must be signed by owner
    traceIfFalse "owner didn't sign" (txSignedBy info $ unPaymentPubKeyHash owner)
    -- must burn beacon
  UpdatePrices newPrice ->
    -- must be signed by owner
    traceIfFalse "owner didn't sign" (txSignedBy info $ unPaymentPubKeyHash owner) &&
    -- must not consume reference script (to save on fees)
    traceIfFalse "updating reference script utxo's datum is not necessary" (null inputsWithRefScripts) &&
    -- datum must be valid price
    traceIfFalse "invalid new asking price" (getPrice newPrice > fromInteger 0) &&
    -- must output to swap script address or owner address
    traceIfFalse "all outputs must go to either the script or the owner" outputsToSelfOrOwner
  Swap ->
    -- must not consume reference script utxo (which also contains beacon)
    traceIfFalse "cannot spend reference script utxo" (null inputsWithRefScripts) &&
    -- ratio met, ADA not withdrawn unless part of swap, and proper datums attached to script outputs
    traceIfFalse "invalid swap" swapCheck

  where
    selfOutputs :: [TxOut]
    selfOutputs = getContinuingOutputs ctx

    outputsToSelfOrOwner :: Bool
    outputsToSelfOrOwner = 
      let isOwnerOutput z = case txOutPubKey z of
            Nothing -> False
            Just pkh -> pkh == unPaymentPubKeyHash owner
      in all (\z -> z `elem` selfOutputs || isOwnerOutput z) $ txInfoOutputs info

    inputsWithRefScripts :: [TxOut]
    inputsWithRefScripts = filter (isJust . txOutReferenceScript)
                         $ map txInInfoResolved
                         $ txInfoInputs info

    -- ValidatorHash of this script
    scriptValidatorHash :: ValidatorHash
    scriptValidatorHash = ownHash ctx

    emptyVal :: Value
    emptyVal = lovelaceValueOf 0

    -- here in case ref script's datum shows up in allDatums
    -- avgAskPrice :: Rational
    -- avgAskPrice =
    --   let allDatums = Map.elems $ txInfoData info
    --       sum' = foldl (\a z -> a + (getPrice $ unsafeFromBuiltinData $ getDatum z)) (fromInteger 0)
    --       numOfDatums = fromInteger $ length allDatums
    --   in sum' allDatums * recip numOfDatums

    -- separate input value to script and rest of input value (can be from other scripts)
    -- fee is subtracted from value of other input
    -- (Valueof script input, value of other input)
    inputValues :: (Value,Value)
    inputValues =
      let inputs = txInfoInputs info
          fee = txInfoFee info
          foo (si,oi) i = case addressCredential $ txOutAddress $ txInInfoResolved i of
            ScriptCredential vh ->
              if vh == scriptValidatorHash
              then (si <> txOutValue (txInInfoResolved i),oi)
              else (si,oi <> txOutValue (txInInfoResolved i))
            PubKeyCredential _ -> (si, oi <> txOutValue (txInInfoResolved i))
      in foldl foo (emptyVal,Num.negate fee) inputs -- accounting for fee paid by user here

    parseDatum :: TxOut -> Price
    parseDatum o = case txOutDatum o of
      (OutputDatum (Datum d)) -> unsafeFromBuiltinData d
      _ -> traceError "Invalid datum for script output"

    -- separate output value to script and rest of output value (can be to other scripts)
    -- throw error if script output doesn't contain proper inline datum (added here to save resources)
    -- (Value of script output, value of other output)
    outputValues :: (Value,Value)
    outputValues =
      let outputs = txInfoOutputs info
          -- emptyVal = lovelaceValueOf 0
          foo acc@(so,oo) o = case (addressCredential $ txOutAddress o,parseDatum o) of
            -- also checks if proper datum is attached
            (ScriptCredential vh,price') ->
              if vh == scriptValidatorHash 
              then if price' == price
                   then (so <> txOutValue o,oo) 
                   else traceError "datum changed in script output"
              else acc
            _ -> (so,oo <> txOutValue o)
      in foldl foo (emptyVal,emptyVal) outputs

    swapCheck :: Bool
    swapCheck =
      let (scriptInValue,otherInValue) = inputValues
          (scriptOutValue,otherOutValue) = outputValues
          scriptValueDiff = scriptInValue <> Num.negate scriptOutValue
          otherValueDiff = otherInValue <> Num.negate otherOutValue
          askedGiven = fromInteger $ uncurry (valueOf scriptValueDiff) askAsset
          offeredTaken = fromInteger $ uncurry (valueOf otherValueDiff) offerAsset
      in if fst askAsset == adaSymbol || fst offerAsset == adaSymbol
         then offeredTaken <= askedGiven * (getPrice price) -- ratio met
         else 
          -- ratio met
          offeredTaken <= askedGiven * (getPrice price) && 
          -- no ada should be swapped
          valueOf scriptValueDiff adaSymbol adaToken == 0 
    

data Swap
instance ValidatorTypes Swap where
  type instance RedeemerType Swap = Action
  type instance DatumType Swap = Price

swap :: BasicInfo -> Validator
swap basicInfo = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Swap
    ($$(PlutusTx.compile [|| mkSwap ||])
      `PlutusTx.applyCode` PlutusTx.liftCode basicInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file 
               . encode 
               . scriptDataToJson ScriptDataJsonDetailedSchema 
               . dataToScriptData 
               . PlutusTx.toData

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                        $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file d = writeJSON file d