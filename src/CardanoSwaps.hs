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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module CardanoSwaps
(
  genBeaconTokenName,
  calcWeightedPrice,
  UtxoPriceInfo (..),
  
  readPubKeyHash,
  readCurrencySymbol,
  readTokenName,

  SwapConfig (..),
  Price,
  Action (..),
  CurrencySymbol(..),
  TokenName(..),
  PaymentPubKeyHash,
  fromGHC,
  BeaconRedeemer (..),
  adaSymbol,
  adaToken,
  StakingConfig (..),

  swapScript,
  stakingScript,
  beaconVaultScript,
  beaconScript,
  beaconSymbol,

  writeScript,
  writeData,

  -- For testing
  swapTypedValidator,
  swapValidator,
  beaconPolicy,
  beaconVaultValidatorHash,
  beaconVault,
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
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
import Ledger.Ada (lovelaceValueOf,lovelaceOf,fromValue)
import PlutusTx.Ratio (fromGHC)
import PlutusTx.AssocMap (keys)
import PlutusPrelude (foldl')
import qualified PlutusTx.AssocMap as Map

-------------------------------------------------
-- Misc Functions
-------------------------------------------------
-- | Create the beacon token name for a trading pair
--
-- The token name is the sha2-256 hash of 
-- "offeredCurrSymbol.offeredTokName/askedCurrSym.askedTokName"
--
-- When ada is part of the pair, that portion of the token name is left blank like:
-- "offeredCurrSymbol.offeredTokName/" or "/askedCurrSym.askedTokName"
genBeaconTokenName :: (BS.ByteString,BS.ByteString) -> (BS.ByteString,BS.ByteString) -> TokenName
genBeaconTokenName (offeredCurrSym,offeredTokName) (askedCurrSym,askedTokName) = 
  let offeredName = 
        if offeredCurrSym Haskell.== BS.empty
        then BS.empty
        else BS.append offeredCurrSym $ BS.append "." offeredTokName
      askedName = 
        if askedCurrSym Haskell.== BS.empty
        then BS.empty
        else BS.append askedCurrSym $ BS.append "." askedTokName
  in TokenName $ sha2_256 $ toBuiltin (BS.append askedName $ BS.append "/" offeredName)

data UtxoPriceInfo = UtxoPriceInfo
  { utxoAmount :: Integer
  , priceNumerator :: Integer
  , priceDenominator :: Integer
  } deriving (Haskell.Show)

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
-- Will match the weighted price calculation done by script.
-- Will throw an error if a price is negative.
calcWeightedPrice :: [UtxoPriceInfo] -> Rational
calcWeightedPrice xs = snd $ foldl' foo (0,fromInteger 0) xs
  where
    convert :: UtxoPriceInfo -> Rational 
    convert upi@UtxoPriceInfo{priceNumerator = num,priceDenominator = den} = 
      case ratio num den of
        Nothing -> Haskell.error $ "Denominator was zero: " <> Haskell.show upi
        Just r ->
          if r <= fromInteger 0
          then Haskell.error $ "Price was not greater than zero: " <> Haskell.show upi
          else r
    
    foo :: (Integer,Rational) -> UtxoPriceInfo -> (Integer,Rational) 
    foo (runningTot,wp) ui@UtxoPriceInfo{..} =
      let !newAmount = runningTot + utxoAmount
          !wp' = ratio' runningTot newAmount * wp +
                ratio' utxoAmount newAmount * convert ui
      in (newAmount,wp')

{-# INLINABLE ratio' #-}
-- | When denominator is zero, returns (fromInteger 0)
ratio' :: Integer -> Integer -> Rational
ratio' num den = if den == 0 then fromInteger 0 else unsafeRatio num den

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

-------------------------------------------------
-- Swap Settings
-------------------------------------------------
-- | For use as extra parameter to the swap script.
-- This creates a unique address for every SwapConfig configuration.
data SwapConfig = SwapConfig
  {
    swapOwner :: PaymentPubKeyHash,
    swapOffer :: (CurrencySymbol,TokenName), -- ^ Asset being offered
    swapAsk :: (CurrencySymbol,TokenName)  -- ^ Asset being asked for
  }

PlutusTx.makeLift ''SwapConfig

-- | Swap Datum
type Price = Rational  -- ^ askedAsset/offeredAsset

-- | Swap Redeemer
data Action 
  -- | Owner can spend any utxo at the script address.
  = Close
  -- | Owner can update all datums at the script address.
  -- The datum with the reference script cannot be updated to conserve fees.
  | UpdatePrices Price
  -- | User can try swapping with assets at the script address.
  | Swap
  -- | Dedicated Redeemer for getting the owner's pub key hash of the script.
  -- This allows for checking the target script has not been tampered with.
  -- To check for tampering: recreate the script using the owner's pkh and assets.
  | Info

PlutusTx.unstableMakeIsData ''Action

-------------------------------------------------
-- Stake Settings
-------------------------------------------------
data StakingConfig = StakingConfig
  { stakeOwner :: PaymentPubKeyHash
  -- | Not used directly by staking script. Only meant to
  -- generate unique staking address if desired.
  , stakeOfferedAsset :: Maybe (CurrencySymbol,TokenName)
  -- | Not used directly by staking script. Only meant to
  -- generate unique staking address if desired.
  , stakeAskedAsset :: Maybe (CurrencySymbol,TokenName) 
  }

PlutusTx.makeLift ''StakingConfig

-------------------------------------------------
-- On-Chain Staking
-------------------------------------------------
-- | Allow any staking related action as long as owner has signed
mkStaking :: StakingConfig -> () -> ScriptContext -> Bool
mkStaking StakingConfig{stakeOwner = owner} () ctx = 
    case purpose of
      Rewarding _  -> 
        traceIfFalse "not signed by owner" (txSignedBy info $ unPaymentPubKeyHash owner)
      Certifying _ -> 
        traceIfFalse "not signed by owner" (txSignedBy info $ unPaymentPubKeyHash owner)
      _            -> False
  where
    purpose :: ScriptPurpose
    purpose = scriptContextPurpose ctx

    info :: TxInfo
    info = scriptContextTxInfo ctx

staking :: StakingConfig -> StakeValidator
staking sc = mkStakeValidatorScript
  ($$(PlutusTx.compile [|| mkUntypedStakeValidator . mkStaking ||])
    `PlutusTx.applyCode` PlutusTx.liftCode sc)

stakingScript :: StakingConfig -> Script
stakingScript = unStakeValidatorScript . staking

-------------------------------------------------
-- Beacon Settings
-------------------------------------------------
-- | Beacon Redeemer
data BeaconRedeemer
  -- | To mint a beacon, 2 ADA must be depositing into the proper script address.
  = MintBeacon TokenName  -- ^ TokenName of token to be minted
  -- | To burn a beacon, 2 ADA must be withdrawn from the proper script address.
  | BurnBeacon TokenName  -- ^ TokenName of token to be burned

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- On-Chain Beacon
-------------------------------------------------
-- | Used to generalize the beacon code.
-- This allows a separate beacon and beacon vault for every application.
-- To force uniqueness, it is used in the error message as the name of the vault.
type AppName = BuiltinString

-- | Script responsible for holding deposits
-- The currency symbol is that of the beacon
mkBeaconVault :: AppName -> CurrencySymbol -> BeaconRedeemer -> ScriptContext -> Bool
mkBeaconVault appName cn r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
   -- | Disallow minting redeemer with beacon vault script
   -- Name used here to force uniqueness.
   MintBeacon _ -> 
    traceError ( "Executing the " 
              <> appName 
              <> " beacon vault script is not necessary for minting a beacon.")
   BurnBeacon tokName ->
     -- | Only allow withdrawing from this script if beacon token also burned.
     traceIfFalse "Must burn one beacon." (mintCheck tokName (-1) minted) &&
     -- | Make sure only 2 ADA is withdrawn.
     -- Only check amount of ADA withdrawn. If other assets are accidentally deposited,
     -- they can be withdrawn along with the 2 ADA.
     traceIfFalse "Must withdraw exactly 2 ADA to burn beacon." 
       (containsOnly2ADA withdrawalValue)

  where
    scriptValidatorHash :: ValidatorHash
    scriptValidatorHash = ownHash ctx

    emptyVal :: Value
    emptyVal = lovelaceValueOf 0

    containsOnly2ADA :: Value -> Bool
    containsOnly2ADA v = lovelaceOf 2_000_000 == fromValue v

    -- | Separate script input value from rest of input value (can be from other scripts).
    -- Throws an error if there is a ref script among script inputs.
    withdrawalValue :: Value
    withdrawalValue =
      let inputs = txInfoInputs info
          foo wVal TxInInfo{txInInfoResolved=TxOut{txOutValue=oVal
                                                  ,txOutReferenceScript=maybeRef
                                                  ,txOutAddress=Address{addressCredential=addrCred}
                                                  }} =
            case (addrCred,maybeRef) of
              (ScriptCredential vh,Nothing) ->
                if vh == scriptValidatorHash
                then wVal <> oVal
                else wVal
              
              (ScriptCredential vh,Just _) ->
                if vh == scriptValidatorHash
                then traceError "Cannot consumer reference script from beacon vault."
                else wVal

              (PubKeyCredential _,_) -> wVal
      in foldl' foo emptyVal inputs

    minted :: [(CurrencySymbol,TokenName,Integer)]
    minted = flattenValue $ txInfoMint info

    mintCheck :: TokenName -> Integer -> [(CurrencySymbol,TokenName,Integer)] -> Bool
    mintCheck tokName target [(cn',tn,n)] = cn == cn' && tn == tokName && n == target
    mintCheck _ _ _ = traceError 
      ( "A beacon token must be burned to withdraw from this vault." <> 
        "\nOnly the beacon token can be burned in this tx."
      )
    

data BeaconVault
instance ValidatorTypes BeaconVault where
  type instance RedeemerType BeaconVault = BeaconRedeemer
  type instance DatumType BeaconVault = CurrencySymbol

beaconVaultTypedValidator :: AppName -> TypedValidator BeaconVault
beaconVaultTypedValidator appName = mkTypedValidator @BeaconVault
    ($$(PlutusTx.compile [|| mkBeaconVault ||])
       `PlutusTx.applyCode` PlutusTx.liftCode appName)
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

beaconVaultValidator :: AppName -> Validator
beaconVaultValidator = Plutonomy.optimizeUPLC . validatorScript . beaconVaultTypedValidator

-- | Change the string to create a different beaconPolicy/beaconVault pair
beaconVault :: Validator
beaconVault = beaconVaultValidator "cardano-swaps-testing"

beaconVaultScript :: Script
beaconVaultScript = unValidatorScript beaconVault

beaconVaultValidatorHash :: ValidatorHash
beaconVaultValidatorHash = Scripts.validatorHash beaconVault

-- | Beacon minting policy
mkBeacon :: ValidatorHash -> BeaconRedeemer -> ScriptContext -> Bool
mkBeacon vaultHash r ScriptContext{scriptContextTxInfo = info} = case r of
   MintBeacon tokName ->
     -- | Must deposit 2 ADA to beacon vault.
     traceIfFalse "Must deposit exactly 2 ADA to the beacon vault." (containsOnly2ADA depositsToVault) &&
     -- | Only one token minted
     traceIfFalse "Wrong number of beacons minted. Only one at a time." (mintCheck tokName 1 minted)

   -- | Delegate checking withdrawal amount to beacon vault script
   BurnBeacon tokName ->
     -- | Beacon vault executed
     traceIfFalse "Must withdraw exactly 2 ADA from the beacon vault." withdrawsFromVault &&
     -- | Only one token burned
     traceIfFalse "Wrong number of beacons burned. Only one at a time." (mintCheck tokName (-1) minted)

  where
    thisCurrencySymbol :: CurrencySymbol
    thisCurrencySymbol =
      let rs = keys $ txInfoRedeemers info
          isMint (Minting _) = True
          isMint _ = False
          Just (Minting cn) = find isMint rs
      in cn

    minted :: [(CurrencySymbol,TokenName,Integer)]
    minted = flattenValue $ txInfoMint info

    mintCheck :: TokenName -> Integer -> [(CurrencySymbol,TokenName,Integer)] -> Bool
    mintCheck tokName target [(_,tn,n)] = tn == tokName && n == target
    mintCheck _ _ _ = traceError "Only the beacon token can be minted/burned in this tx."

    -- | Check if utxo belongs to beacon vault
    checkHash :: TxOut -> Bool
    checkHash TxOut{txOutAddress=Address{addressCredential=ScriptCredential h}} = vaultHash == h
    checkHash _ = False

    -- | Check if any tx inputs go to the beacon vault
    withdrawsFromVault :: Bool
    withdrawsFromVault = any (checkHash . txInInfoResolved) $ txInfoInputs info

    emptyVal :: Value
    emptyVal = lovelaceValueOf 0

    containsOnly2ADA :: Value -> Bool
    containsOnly2ADA v = lovelaceOf 2_000_000 == fromValue v

    parseDatum :: OutputDatum -> CurrencySymbol
    parseDatum d = case d of
      (OutputDatum (Datum d')) -> case fromBuiltinData d' of
        Nothing -> traceError "Beacon vault datum must be the beacon policy id."
        Just p -> p
      _ -> traceError "Deposits to beacon vault must include an inline datum."

    -- | Find deposits to beacon vault
    depositsToVault :: Value
    depositsToVault = 
      let outputs = txInfoOutputs info
          foo dVal TxOut{txOutValue=oVal
                        ,txOutDatum=d
                        ,txOutAddress=Address{addressCredential=addrCred}} =
            case addrCred of
              ScriptCredential vh ->
                if vh == vaultHash
                then -- | Check if output to beacon vualt script contains proper datum.
                     if parseDatum d == thisCurrencySymbol
                     then dVal <> oVal
                     else traceError "The beacon vault datum is not the beacon policy id."
                else dVal -- ^ Skip this output.

              PubKeyCredential _ -> dVal  -- ^ Skip this output.
      in foldl' foo emptyVal outputs

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
-- On-Chain Swap
-------------------------------------------------
-- | The price is not used individually. Instead it is obtain by traversing the script context.
mkSwap :: CurrencySymbol -> SwapConfig -> Price -> Action -> ScriptContext -> Bool
mkSwap beaconSym SwapConfig{..} _ action ctx@ScriptContext{scriptContextTxInfo = info} = case action of
  Info -> traceError ("Owner's payment pubkey hash:\n" <> ownerAsString)
  Close ->
    -- | Must be signed by owner.
    traceIfFalse "owner didn't sign" (txSignedBy info $ unPaymentPubKeyHash swapOwner) &&
    -- | If reference script consumed, must burn beacon.
    traceIfFalse "If reference script is consumed, the beacon must be burned." 
      ( if not noInputsWithRefScripts
        then beaconBurned
        else outputCheck Nothing
      )
  UpdatePrices newPrice ->
    -- | Must be signed by owner.
    traceIfFalse "owner didn't sign" (txSignedBy info $ unPaymentPubKeyHash swapOwner) &&
    -- | Must not consume reference script (to save on fees).
    traceIfFalse "updating reference script utxo's datum is not necessary" noInputsWithRefScripts &&
    -- | Datum must be valid price. Any number great than zero.
    traceIfFalse "invalid new asking price" (newPrice > fromInteger 0) &&
    -- | All outputs must contain same datum as specified in redeemer
    -- Must output to swap script address or owner address.
    -- Cannot remove beacon from swap address.
    traceIfFalse "New price datums do not match price supplied in redeemer." (outputCheck $ Just newPrice)
  Swap ->
    -- | Should not consume reference script from swap script address.
    -- Utxo output to the script must have the proper datum and datum must not differ from input.
    -- Only offered asset should leave the swap script address.
    -- User must supply the ADA for each utxo with native tokens.
    -- Max offered asset taken <= given asset * price.
    swapCheck

  where
    ownerAsString :: BuiltinString
    ownerAsString = decodeUtf8 $ getPubKeyHash $ unPaymentPubKeyHash swapOwner

    beaconBurned :: Bool
    beaconBurned = 
      let burned = flattenValue $ txInfoMint info
          mintCheck (cn,_,n) = cn == beaconSym && n == (-1)
      in any mintCheck burned

    -- | ValidatorHash of this script
    scriptValidatorHash :: ValidatorHash
    scriptValidatorHash = ownHash ctx

    parseDatum :: BuiltinString -> OutputDatum -> Price
    parseDatum err d = case d of
      (OutputDatum (Datum d')) -> case fromBuiltinData d' of
        Nothing -> traceError err
        Just p -> p
      _ -> traceError "All datums must be inline datums."

    noInputsWithRefScripts :: Bool
    noInputsWithRefScripts = not $ any (isJust . txOutReferenceScript)
                         $ map txInInfoResolved
                         $ txInfoInputs info

    -- | Checks outputs for both Close and Update redeemers
    outputCheck :: Maybe Price -> Bool
    outputCheck newPrice' =
      let outputs = txInfoOutputs info
          foo TxOut{txOutDatum=d,txOutValue=oVal,txOutAddress=Address{addressCredential=addrCred}} =
            case addrCred of
              ScriptCredential vh ->
                -- | Check if script is this script.
                if vh == scriptValidatorHash
                then case newPrice' of
                  -- | Make sure proper datum present to prevent accidental locking
                  Nothing -> parseDatum "Invalid datum in swap output" d > fromInteger 0
                  Just newPrice -> 
                     -- | Check if datum has proper price.
                     -- This will throw an error if datum is not an inline datum for price.
                     parseDatum "Invalid datum in swap output." d == newPrice
                else case Map.lookup beaconSym (getValue oVal) of
                  Nothing -> True  -- ^ Fine since owner signed.
                  Just _ -> traceError "Cannot withdraw beacon from swap address."

              PubKeyCredential _ -> 
                -- | Allow as long as beacon not withdrawn from swap address. 
                case Map.lookup beaconSym (getValue oVal) of
                  Nothing -> True  -- ^ Fine since owner signed.
                  Just _ -> traceError "Cannot withdraw beacon from swap address."
      in all foo outputs

    emptyVal :: Value
    emptyVal = lovelaceValueOf 0

    -- | Separate script input value from rest of input value (can be from other scripts).
    -- Throws an error if there is a ref script among script inputs.
    -- Get the weighted average price for all the utxo inputs from this script. This will
    -- throw an error if the datum is not an inline datum or if it is not a price.
    --
    -- returns (Total Value from Script,Weighted Price)
    swapInputInfo :: (Value,Price)
    swapInputInfo =
      let inputs = txInfoInputs info
          foo x@(sValue,(taken,wp)) TxInInfo{txInInfoResolved=TxOut{txOutValue=iVal
                                                                   ,txOutDatum=d
                                                                   ,txOutReferenceScript=maybeRef
                                                                   ,txOutAddress=Address{addressCredential=addrCred}
                                                                   }} =
            case (addrCred,maybeRef) of
              (ScriptCredential vh,Nothing) ->
                if vh == scriptValidatorHash
                then 
                  let iTaken = uncurry (valueOf iVal) swapOffer
                      price' = parseDatum "Invalid datum in swap input." d
                      !newTaken = taken + iTaken
                      !newWeightedPrice =
                        ratio' taken newTaken * wp +
                        ratio' iTaken newTaken * price'
                      !newSwapValue = sValue <> iVal
                  in -- | Fail if the price is not > 0.
                     if price' <= fromInteger 0
                     then traceError "All prices must be greater than 0."
                     else ( newSwapValue
                          , (newTaken,newWeightedPrice)
                          )
                else x  -- ^ Skip this input
                         
              (ScriptCredential vh,Just _) ->
                if vh == scriptValidatorHash
                then traceError "Cannot consume reference script from swap address."
                else x  -- ^ Skip this input

              (PubKeyCredential _,_) -> x  -- ^ skip this input
      in fmap snd $ foldl' foo (emptyVal,(0,fromInteger 0)) inputs

    -- | Separate output value to script from rest of output value (can be to other scripts).
    -- Throw error if output to script doesn't contain proper inline datum.
    -- The supplied price is the weighted average of all script input prices.
    scriptOutputValue :: Price -> Value
    scriptOutputValue weightedPrice =
      let outputs = txInfoOutputs info
          foo sValue TxOut{txOutDatum=d
                          ,txOutValue=oVal
                          ,txOutAddress=Address{addressCredential=addrCred}} =
            case addrCred of
              ScriptCredential vh -> 
                -- | Check if it belongs to the swap script.
                if vh == scriptValidatorHash
                then -- | Check if output to swap script contains proper datum.
                     if parseDatum "Invalid datum in swap output." d == weightedPrice
                     then sValue <> oVal
                     else traceError "Output datum is not the weighted price of all utxo inputs."
                else sValue  -- ^ Skip this output.

              PubKeyCredential _ -> sValue  -- ^ Skip this output.
      in foldl' foo emptyVal outputs

    swapCheck :: Bool
    swapCheck =
      let -- | Input info
          (scriptInputValue,weightedPrice) = swapInputInfo

          -- | Convert price if necessary
          correctedPrice
            -- | If ADA is offered, divide the weighted price by 1,000,000
            | swapOffer == (adaSymbol,adaToken) = weightedPrice * ratio' 1 1_000_000
            -- | If ADA is asked, multiply the weighted price by 1,000,000
            | swapAsk == (adaSymbol,adaToken) = weightedPrice * ratio' 1_000_000 1
            | otherwise = weightedPrice
          
          -- | Value differences
          scriptValueDiff 
            =  scriptOutputValue weightedPrice -- ^ checks if outputs contain weighted price
            <> Num.negate scriptInputValue

          -- | Amounts
          askedGiven = fromInteger $ uncurry (valueOf scriptValueDiff) swapAsk
          offeredTaken = fromInteger $ Num.negate $ uncurry (valueOf scriptValueDiff) swapOffer

          -- | Assets leaving script address
          leavingAssets = flattenValue $ fst $ split scriptValueDiff -- zero diff amounts removed
          isOnlyOfferedAsset [(cn,tn,_)] = (cn,tn) == swapOffer
          isOnlyOfferedAsset [] = True -- ^ allows consolidating utxos at swap script address
          isOnlyOfferedAsset _ = False
      in
        -- | Only the offered asset is allowed to leave the script address.
        -- When ADA is not being offered, the user is required to supply the ADA for native token utxos.
        -- This means that, when ADA is not offered, the script's ADA value can only increase.
        traceIfFalse "Only the offered asset is allowed to leave." (isOnlyOfferedAsset leavingAssets) &&

        -- | Ratio sets the maximum amount of the offered asset that can be taken.
        -- To withdraw more of the offered asset, more of the asked asset must be deposited to the script.
        -- Uses the corrected price to account for converting ADA to lovelace 
        traceIfFalse "Not enough of the asked asset given: offeredTaken * price <= askedGiven" (offeredTaken * correctedPrice <= askedGiven)

data Swap
instance ValidatorTypes Swap where
  type instance RedeemerType Swap = Action
  type instance DatumType Swap = Price

swapTypedValidator :: CurrencySymbol -> SwapConfig -> TypedValidator Swap
swapTypedValidator beaconSym swapConfig = mkTypedValidator
    ($$(PlutusTx.compile [|| mkSwap ||])
      `PlutusTx.applyCode` PlutusTx.liftCode beaconSym
      `PlutusTx.applyCode` PlutusTx.liftCode swapConfig)
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

swapValidator :: CurrencySymbol -> SwapConfig -> Validator
swapValidator beaconSym swapConfig = Plutonomy.optimizeUPLC 
                                   $ validatorScript 
                                   $ swapTypedValidator beaconSym swapConfig

swapScript :: SwapConfig -> Script
swapScript = unValidatorScript . swapValidator beaconSymbol

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
writeData = writeJSON