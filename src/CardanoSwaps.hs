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
import Ledger.Value (valueOf,split,flattenValue)
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
    -- should not consume reference script from swap script address
    -- utxo output to the script must have the proper datum and datum must not differ from input
    -- only offered asset should leave the swap script address
    -- max offered asset taken <= given asset * price
    traceIfFalse ("invalid swap:" 
               <> "\nShould not consume reference script from swap address"
               <> "\nUtxo output to swap address must contain proper datum (same as input)"
               <> "\nOnly the offered asset is allowed to leave the swap address"
               <> "\nOffered asset leaving <= Asked asset given * price") swapCheck

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

    -- separate script input value from rest of input value (can be from other scripts)
    -- throws an error if there is a ref script among script inputs
    scriptInputValue :: Value
    scriptInputValue =
      let inputs = txInfoInputs info
          foo si i = case addressCredential $ txOutAddress $ txInInfoResolved i of
            ScriptCredential vh ->
              -- check if it belongs to swap script
              if vh == scriptValidatorHash
              then -- check if it contains a ref script from swap script address 
                   if isJust $ txOutReferenceScript $ txInInfoResolved i
                   then traceError "Cannot consumer reference script from swap address"
                   else si <> txOutValue (txInInfoResolved i)
              else si
            PubKeyCredential _ -> si
      in foldl foo emptyVal inputs

    parseDatum :: TxOut -> Price
    parseDatum o = case txOutDatum o of
      (OutputDatum (Datum d)) -> unsafeFromBuiltinData d
      _ -> traceError "Invalid datum for swap script output"

    -- separate output value to script from rest of output value (can be to other scripts)
    -- throw error if output to script doesn't contain proper inline datum
    scriptOutputValue :: Value
    scriptOutputValue =
      let outputs = txInfoOutputs info
          foo so o = case (addressCredential $ txOutAddress o,parseDatum o) of
            -- also checks if proper datum is attached
            (ScriptCredential vh,price') ->
              -- check if it belongs to swap script
              if vh == scriptValidatorHash 
              then -- check if output to swap script contains proper datum
                   if price' == price
                   then so <> txOutValue o
                   else traceError "datum changed in script output"
              else so
            _ -> so
      in foldl foo emptyVal outputs

    swapCheck :: Bool
    swapCheck =
      let  -- value differences
          scriptValueDiff = scriptOutputValue <> Num.negate scriptInputValue

          -- amounts
          askedGiven = fromInteger $ uncurry (valueOf scriptValueDiff) askAsset
          offeredTaken = fromInteger $ Num.negate $ uncurry (valueOf scriptValueDiff) offerAsset

          -- assets leaving script address
          leavingAssets = flattenValue $ fst $ split scriptValueDiff -- zero diff amounts removed
          isOnlyOfferedAsset [(cn,tn,_)] = (cn,tn) == offerAsset
          isOnlyOfferedAsset           _ = False
      in
        -- only the offered asset is allowed to leave the script address
        -- when ADA is not being offered, the user is required to supply the 1 ADA for native token utxos
        -- this means that, when ADA is not offered, the script's ADA value can only increase
        isOnlyOfferedAsset leavingAssets &&

        -- ratio sets the maximum amount of the offered asset that can be taken
        -- to withdraw more of the offered asset, more of the asked asset must be deposited to the script
        offeredTaken <= askedGiven * (getPrice price)
    

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