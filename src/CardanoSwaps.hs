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
  genPairTokenName,
  calcWeightedPrice,
  UtxoPriceInfo (..),
  
  readPubKeyHash,
  readCurrencySymbol,
  readTokenName,

  BasicInfo (..),
  Price,
  Action (..),
  CurrencySymbol(..),
  TokenName(..),
  PaymentPubKeyHash,
  fromGHC,
  BeaconRedeemer (..),
  adaSymbol,
  adaToken,

  swapScript,
  beaconVaultScript,
  beaconScript,
  beaconSymbol,

  writeScript,
  writeData,
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
import qualified Prelude as Haskell
import Data.String (fromString)
import Control.Monad (mzero)

import           Cardano.Api hiding (Script,Value,TxOut)
import           Cardano.Api.Shelley   (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,split,flattenValue)
import PlutusTx.Numeric as Num
import Plutus.V2.Ledger.Tx
import Ledger.Ada (lovelaceValueOf,lovelaceOf,fromValue)
import PlutusTx.Ratio (fromGHC)

-------------------------------------------------
-- Misc Functions
-------------------------------------------------
genPairTokenName :: CurrencySymbol -> TokenName -> CurrencySymbol -> TokenName -> TokenName
genPairTokenName offeredCurrSym offeredTokName askedCurrSym askedTokName = 
  let offeredName = unCurrencySymbol offeredCurrSym <> unTokenName offeredTokName
      askedName = unCurrencySymbol askedCurrSym <> unTokenName askedTokName
  in TokenName $ sha3_256 (offeredName <> "/" <> askedName)

data UtxoPriceInfo = UtxoPriceInfo
  { utxoAmount :: Integer
  , priceNumerator :: Integer
  , priceDenominator :: Integer
  }

instance FromJSON UtxoPriceInfo where
  parseJSON (Object o) = 
    UtxoPriceInfo 
      Haskell.<$> o .: "utxoAmount"
      Haskell.<*> o .: "priceNumerator"
      Haskell.<*> o .: "priceDenominator"
  parseJSON _ = mzero

instance ToJSON UtxoPriceInfo where
  toJSON UtxoPriceInfo{..} =
    object [ "utxoAmount" .= utxoAmount
           , "priceNumerator" .= priceNumerator
           , "priceDenominator" .= priceDenominator
           ]

-- | Helper function to calculate the weighted price.
--   Will match the weighted price calculation done by script.
--   Meant to be used with a UtxoPriceInfo JSON File.
calcWeightedPrice :: [UtxoPriceInfo] -> (Integer,Rational)
calcWeightedPrice xs = foldl foo (0,fromInteger 0) xs
  where
    convert :: (UtxoPriceInfo) -> Rational 
    convert UtxoPriceInfo{priceNumerator = num,priceDenominator = den} = 
      case ratio num den of
        Nothing -> Haskell.error $ "Denominator was zero: " <> Haskell.show (num,den)
        Just r -> r
    
    foo :: (Integer,Rational) -> UtxoPriceInfo -> (Integer,Rational) 
    foo (runningTot,wp) ui@UtxoPriceInfo{..} =
      let newAmount = runningTot + utxoAmount
          wp' = ratio' runningTot newAmount * wp +
                ratio' utxoAmount newAmount * convert ui
      in (newAmount,wp')

{-# INLINABLE ratio' #-}
-- | When denominator is zero, returns (fromInteger 0)
ratio' :: Integer -> Integer -> Rational
ratio' num den = if den == 0 then fromInteger 0 else unsafeRatio num den

-------------------------------------------------
-- Swap Settings
-------------------------------------------------
-- | For use as extra parameter to the swap script.
--   This creates a unique address for every BasicInfo configuration.
data BasicInfo = BasicInfo
  {
    owner :: PaymentPubKeyHash,
    offerAsset :: (CurrencySymbol,TokenName),
    askAsset :: (CurrencySymbol,TokenName)
  }

PlutusTx.makeLift ''BasicInfo

-- | Parse PaymentPubKeyHash from user supplied String
readPubKeyHash :: Haskell.String -> Either Haskell.String PaymentPubKeyHash
readPubKeyHash s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ PaymentPubKeyHash $ PubKeyHash bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse Currency from user supplied String
readCurrencySymbol :: Haskell.String -> Either Haskell.String CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ CurrencySymbol bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse TokenName from user supplied String
readTokenName :: Haskell.String -> Either Haskell.String TokenName
readTokenName s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TokenName bytes'
  Left msg                   -> Left $ "could not convert: " <> msg


-- | Swap Datum
type Price = Rational  -- ^ askedAsset/offeredAsset

-- | Swap Redeemer
data Action 
  -- | Owner can spend any utxo at the script address.
  = Close
  -- | Owner can update all datums at the script address.
  --   The datum with the reference script cannot be updated to conserve fees.
  | UpdatePrices Price
  -- | User can try swapping with assets at the script address.
  | Swap
  -- | Dedicated Redeemer for getting the owner's pub key hash of the script.
  --   This allows for checking the target script has not been tampered with.
  --   To check for tampering: recreate the script using the owner's pkh and assets.
  | Info

PlutusTx.unstableMakeIsData ''Action

-------------------------------------------------
-- On-Chain Swap
-------------------------------------------------
mkSwap :: BasicInfo -> Price -> Action -> ScriptContext -> Bool
mkSwap BasicInfo{..} price action ctx@ScriptContext{scriptContextTxInfo = info} = case action of
  Info -> traceError ("Owner's payment pubkey hash:\n" <> ownerAsString)
  Close ->
    -- | Must be signed by owner.
    traceIfFalse "owner didn't sign" (txSignedBy info $ unPaymentPubKeyHash owner)
  UpdatePrices newPrice ->
    -- | Must be signed by owner.
    traceIfFalse "owner didn't sign" (txSignedBy info $ unPaymentPubKeyHash owner) &&
    -- | Must not consume reference script (to save on fees).
    traceIfFalse "updating reference script utxo's datum is not necessary" (null inputsWithRefScripts) &&
    -- | Datum must be valid price. Any number great than zero.
    traceIfFalse "invalid new asking price" (newPrice > fromInteger 0) &&
    -- | All outputs must contain same datum as specified in redeemer
    traceIfFalse "new datums do not match price in redeemer" (allDatumsMatchRedeemerPrice newPrice) &&
    -- | Must output to swap script address or owner address.
    traceIfFalse "all outputs must go to either the script or the owner" outputsToSelfOrOwner
  Swap -> 
    -- | Should not consume reference script from swap script address.
    --   Utxo output to the script must have the proper datum and datum must not differ from input.
    --   Only offered asset should leave the swap script address.
    --   User must supply the 1 ADA for each utxo with native tokens.
    --   Max offered asset taken <= given asset * price.
    traceIfFalse ("Invalid swap:" 
              --  <> "\nShould not consume reference script from swap address"
              --  <> "\nUtxo output to swap address must contain proper datum (must match input datum)"
               <> "\nOnly the offered asset is allowed to leave the swap address."
               <> "\nUser must supply the extra ADA (if necessary) for each output with native tokens."
               <> "\nOffered asset leaving <= Asked asset given * price") swapCheck

  where
    ownerAsString :: BuiltinString
    ownerAsString = decodeUtf8 $ getPubKeyHash $ unPaymentPubKeyHash $ owner

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

    -- | Check datums of new outputs to script. If new datum is not an inline datum,
    --   it will throw an error.
    allDatumsMatchRedeemerPrice :: Price -> Bool
    allDatumsMatchRedeemerPrice newPrice = 
      let outputs = txInfoOutputs info
          foo o = case (addressCredential $ txOutAddress o, parseDatum outputDatumError o) of
            -- | if output is to the script, check its datum too
              (ScriptCredential vh,price') -> 
                if vh == scriptValidatorHash
                then -- | Check if output to swap script contains proper datum.
                     price' == newPrice
                else True -- ^ If output to other script, ignore datum
              _ -> True -- ^ If output to user wallet, ignore datum
      in all foo outputs

    outputDatumError :: BuiltinString
    outputDatumError = "Invalid price datum for swap script output. Must be inline datum."

    inputDatumError :: BuiltinString
    inputDatumError = "Failed to parse input datum."

    parseDatum :: BuiltinString -> TxOut -> Price
    parseDatum err o = case txOutDatum o of
      (OutputDatum (Datum d)) -> unsafeFromBuiltinData d
      _ -> traceError err

    -- | ValidatorHash of this script
    scriptValidatorHash :: ValidatorHash
    scriptValidatorHash = ownHash ctx

    emptyVal :: Value
    emptyVal = lovelaceValueOf 0

    -- | How much of the offered asset is available in this utxo input and for what price.
    priceTier :: TxOut -> (Integer,Price)
    priceTier o = 
      ( valueOf (txOutValue o) (fst offerAsset) (snd offerAsset)
      , parseDatum inputDatumError o
      )
    
    -- | Separate script input value from rest of input value (can be from other scripts).
    --   Throws an error if there is a ref script among script inputs.
    --   Get the weighted average price for all the utxo inputs from this script. This will
    --   throw an error if the datum is not an inline datum or if it is not a price.
    --
    --   returns (Total Value from Script,(Total Offered Asset in Inputs,Weighted Price))
    scriptInputInfo :: (Value,(Integer,Price))
    scriptInputInfo =
      let inputs = txInfoInputs info
          addrCred i = addressCredential $ txOutAddress $ txInInfoResolved i
          foo (si,(on,wp)) i = 
            -- | Check if input belongs to this script
            if ScriptCredential scriptValidatorHash == addrCred i
            then -- | check if input contains a ref script from the swap script
                 if isJust $ txOutReferenceScript $ txInInfoResolved i
                 then traceError "Cannot consume reference script from swap address."
                 else let (offeredInInput,price') = priceTier $ txInInfoResolved i
                          newAmount = on + offeredInInput
                          newWeightedPrice =
                            ratio' on newAmount * wp +
                            ratio' offeredInInput newAmount * price'
                      in ( si <> txOutValue (txInInfoResolved i)
                         , (newAmount,newWeightedPrice)
                         )
            else (si,(on,wp))
      in foldl foo (emptyVal,(0,fromInteger 0)) inputs

    -- | Separate script input value from rest of input value (can be from other scripts).
    --   Throws an error if there is a ref script among script inputs.
    -- scriptInputValue :: Value
    -- scriptInputValue =
    --   let inputs = txInfoInputs info
    --       foo si i = case addressCredential $ txOutAddress $ txInInfoResolved i of
    --         ScriptCredential vh ->
    --           -- check if it belongs to swap script
    --           if vh == scriptValidatorHash
    --           then -- check if it contains a ref script from swap script address 
    --                if isJust $ txOutReferenceScript $ txInInfoResolved i
    --                then traceError "Cannot consume reference script from swap address"
    --                else si <> txOutValue (txInInfoResolved i)
    --           else si
    --         PubKeyCredential _ -> si
    --   in foldl foo emptyVal inputs

    -- | Separate output value to script from rest of output value (can be to other scripts).
    --   Throw error if output to script doesn't contain proper inline datum.
    --   The supplied price is the weighted average of all script input prices.
    scriptOutputValue :: Price -> Value
    scriptOutputValue weightedPrice =
      let outputs = txInfoOutputs info
          foo so o = case addressCredential $ txOutAddress o of
            -- | Also checks if proper datum is attached.
            ScriptCredential vh ->
              -- | check if it belongs to swap script
              if vh == scriptValidatorHash 
              then -- | Check if output to swap script contains proper datum.
                   --   Throws error if invalid datum.
                   if parseDatum outputDatumError o == weightedPrice
                   then so <> txOutValue o
                   else traceError "output datum is not the weighted price of all utxo inputs"
              else so
            _ -> so
      in foldl foo emptyVal outputs


    -- | Separate output value to script from rest of output value (can be to other scripts).
    --   Throw error if output to script doesn't contain proper inline datum.
    -- scriptOutputValue :: Value
    -- scriptOutputValue =
    --   let outputs = txInfoOutputs info
    --       foo so o = case (addressCredential $ txOutAddress o,parseDatum o) of
    --         -- | Also checks if proper datum is attached.
    --         (ScriptCredential vh,price') ->
    --           -- check if it belongs to swap script
    --           if vh == scriptValidatorHash 
    --           then -- | Check if output to swap script contains proper datum.
    --                if price' == price
    --                then so <> txOutValue o
    --                else traceError "datum changed in script output"
    --           else so
    --         _ -> so
    --   in foldl foo emptyVal outputs

    swapCheck :: Bool
    swapCheck =
      let -- | Input info
          (scriptInputValue,(_,weightedPrice)) = scriptInputInfo

          -- | Convert price if necessary
          correctedPrice
            -- | If ADA is offered, divide the weighted price by 1,000,000
            | offerAsset == (adaSymbol,adaToken) = weightedPrice * ratio' 1 1_000_000
            -- | If ADA is asked, multiply the weighted price by 1,000,000
            | askAsset == (adaSymbol,adaToken) = weightedPrice * ratio' 1_000_000 1
            | otherwise = weightedPrice
          
          -- | Value differences
          scriptValueDiff 
            =  scriptOutputValue weightedPrice -- ^ checks if outputs contain weighted price
            <> Num.negate scriptInputValue

          -- | Amounts
          askedGiven = fromInteger $ uncurry (valueOf scriptValueDiff) askAsset
          offeredTaken = fromInteger $ Num.negate $ uncurry (valueOf scriptValueDiff) offerAsset

          -- | Assets leaving script address
          leavingAssets = flattenValue $ fst $ split scriptValueDiff -- zero diff amounts removed
          isOnlyOfferedAsset [(cn,tn,_)] = (cn,tn) == offerAsset
          isOnlyOfferedAsset [] = True -- ^ allows consolidating utxos at swap script address
          isOnlyOfferedAsset _ = False
      in
        -- | Only the offered asset is allowed to leave the script address.
        --   When ADA is not being offered, the user is required to supply the ADA for native token utxos.
        --   This means that, when ADA is not offered, the script's ADA value can only increase.
        isOnlyOfferedAsset leavingAssets &&

        -- | Ratio sets the maximum amount of the offered asset that can be taken.
        --   To withdraw more of the offered asset, more of the asked asset must be deposited to the script.
        --   Uses the corrected price to account for converting ADA to lovelace 
        offeredTaken * (correctedPrice) <= askedGiven

    -- swapCheck :: Bool
    -- swapCheck =
    --   let  -- | Value differences
    --       scriptValueDiff = scriptOutputValue <> Num.negate scriptInputValue

    --       -- | Amounts
    --       askedGiven = fromInteger $ uncurry (valueOf scriptValueDiff) askAsset
    --       offeredTaken = fromInteger $ Num.negate $ uncurry (valueOf scriptValueDiff) offerAsset

    --       -- | Assets leaving script address
    --       leavingAssets = flattenValue $ fst $ split scriptValueDiff -- zero diff amounts removed
    --       isOnlyOfferedAsset [(cn,tn,_)] = (cn,tn) == offerAsset
    --       isOnlyOfferedAsset           _ = False
    --   in
    --     -- | Only the offered asset is allowed to leave the script address.
    --     --   When ADA is not being offered, the user is required to supply the ADA for native token utxos.
    --     --   This means that, when ADA is not offered, the script's ADA value can only increase.
    --     isOnlyOfferedAsset leavingAssets &&

    --     -- | Ratio sets the maximum amount of the offered asset that can be taken.
    --     --   To withdraw more of the offered asset, more of the asked asset must be deposited to the script.
    --     --   Uses the corrected price to account for converting ADA to lovelace 
    --     offeredTaken * (correctedPrice) <= askedGiven
    

data Swap
instance ValidatorTypes Swap where
  type instance RedeemerType Swap = Action
  type instance DatumType Swap = Price

swapValidator :: BasicInfo -> Validator
swapValidator basicInfo = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Swap
    ($$(PlutusTx.compile [|| mkSwap ||])
      `PlutusTx.applyCode` PlutusTx.liftCode basicInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

swapScript :: BasicInfo -> Script
swapScript = unValidatorScript . swapValidator

-------------------------------------------------
-- Beacon Settings
-------------------------------------------------
-- | Beacon Redeemer
data BeaconRedeemer
  -- | To mint a beacon, 1 ADA must be depositing into the proper script address.
  = MintBeacon
  -- | To burn a beacon, 1 ADA must be withdrawn from the proper script address.
  | BurnBeacon

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- On-Chain Beacon
-------------------------------------------------
-- | Script responsible for holding deposits
mkBeaconVault :: () -> BeaconRedeemer -> ScriptContext -> Bool
mkBeaconVault () r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
   MintBeacon ->
     -- | Only allow minting if 1 ADA also deposited at this script in same tx.
     traceIfFalse "Must deposit exactly 1 ADA to mint beacon. Must deposit and mint in same tx." 
       (containsOnly1ADA deposit)
   BurnBeacon ->
     -- | Only allow burning if 1 ADA is also withdrawn from this script.
     --   Only check amount of ADA withdrawn. If other assets are accidentally deposited,
     --   they can be withdrawn along with the 1 ADA.
     traceIfFalse "Must withdraw exactly 1 ADA to burn beacon. Must withdraw and burn in same tx." 
       (containsOnly1ADA withdrawal)

  where
    scriptValidatorHash :: ValidatorHash
    scriptValidatorHash = ownHash ctx

    deposit :: Value
    deposit = valueLockedBy info scriptValidatorHash

    containsOnly1ADA :: Value -> Bool
    containsOnly1ADA v = lovelaceOf 1_000_000 == fromValue v

    withdrawal :: Value
    withdrawal = fold $ map snd $ scriptOutputsAt scriptValidatorHash info

data BeaconVault
instance ValidatorTypes BeaconVault where
  type instance RedeemerType BeaconVault = BeaconRedeemer
  type instance DatumType BeaconVault = ()

beaconVaultValidator :: Validator
beaconVaultValidator = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @BeaconVault
    $$(PlutusTx.compile [|| mkBeaconVault ||])
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

beaconVaultScript :: Script
beaconVaultScript = unValidatorScript beaconVaultValidator

beaconVaultValidatorHash :: ValidatorHash
beaconVaultValidatorHash = Scripts.validatorHash beaconVaultValidator

-- | Beacon minting policy
mkBeacon :: ValidatorHash -> BeaconRedeemer -> ScriptContext -> Bool
mkBeacon vaultHash r ScriptContext{scriptContextTxInfo = info} = case r of
   MintBeacon ->
     -- | Must deposit 1 ADA to proper script address.
     traceIfFalse ("Must deposit 1 ADA to this script address: " <> hashToString vaultHash)
       (containsOnly1ADA deposit)
   BurnBeacon ->
     -- | Must withdraw 1 ADA from proper script address.
     traceIfFalse ("Must withdraw 1 ADA from this script address: " <> hashToString vaultHash)
       (containsOnly1ADA withdrawal)

  where
    hashToString :: ValidatorHash -> BuiltinString
    hashToString (ValidatorHash vh) = decodeUtf8 vh 

    deposit :: Value
    deposit = valueLockedBy info vaultHash

    containsOnly1ADA :: Value -> Bool
    containsOnly1ADA v = lovelaceOf 1_000_000 == fromValue v

    withdrawal :: Value
    withdrawal = fold $ map snd $ scriptOutputsAt vaultHash info

beaconPolicy :: ValidatorHash -> MintingPolicy
beaconPolicy vh = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode vh)
  where
    wrap = mkUntypedMintingPolicy . mkBeacon

beaconScript :: Script
beaconScript = unMintingPolicyScript $ beaconPolicy beaconVaultValidatorHash

beaconSymbol :: CurrencySymbol
beaconSymbol = scriptCurrencySymbol $ beaconPolicy beaconVaultValidatorHash

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