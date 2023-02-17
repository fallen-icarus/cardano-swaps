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
  SwapConfig(..),
  Price,
  Action(..),
  SwapDatum(..),
  BeaconRedeemer(..),
  CurrencySymbol(..),
  TokenName(..),
  PaymentPubKeyHash(..),
  adaSymbol,
  adaToken,

  UtxoPriceInfo(..),
  fromGHC,
  calcWeightedPrice,
  readCurrencySymbol,
  readTokenName,

  swapValidator,
  swapValidatorScript,
  swapValidatorHash,
  
  beaconPolicy,
  beaconScript,
  beaconSymbol,

  writeScript,
  writeData
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath,seq) 
import qualified Prelude as Haskell
import Data.String (fromString)

import Cardano.Api hiding (Address,Script,Value,TxOut)
import Cardano.Api.Shelley (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api as PlutusApi
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,split,flattenValue)
import PlutusTx.Numeric as Num
import PlutusTx.Ratio (fromGHC)
import PlutusPrelude (foldl')
import qualified PlutusTx.AssocMap as Map

-------------------------------------------------
-- Off-Chain Helper Functions
-------------------------------------------------
data UtxoPriceInfo = UtxoPriceInfo
  { utxoAmount :: Integer
  , priceNumerator :: Integer
  , priceDenominator :: Integer
  } deriving (Haskell.Show)

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
-- On-Chain Helper Functions
-------------------------------------------------
{-# INLINABLE ratio' #-}
-- | When denominator is zero, returns (fromInteger 0)
ratio' :: Integer -> Integer -> Rational
ratio' num den = if den == 0 then fromInteger 0 else unsafeRatio num den

-------------------------------------------------
-- Swap Settings
-------------------------------------------------
-- | Extra parameter for the spending script.
-- This helps ensure only the target assets are swapped and creates a unique beacon for
-- each trading pair.
data SwapConfig = SwapConfig
  { swapOffer :: (CurrencySymbol,TokenName)
  , swapAsk :: (CurrencySymbol,TokenName)
  }

PlutusTx.makeLift ''SwapConfig

-- | Swap Datum
type Price = Rational  -- ^ askedAsset/offeredAsset
data SwapDatum = SwapDatum
  { swapPrice :: Price
  , swapBeacon :: Maybe CurrencySymbol  -- ^ Currency symbol for the beacon.
  }

-- | Swap Redeemer
data Action
  -- | Close
  -- Requirements:
  -- 1) All beacons in inputs must be burned.
  --    - since beacons must be stored with the reference script, this also means you cannot
  --      remove the main reference script while leaving behind the beacon.
  -- 2) Staking credential signed or was executed.
  -- 3) All outputs to address contain proper datum.
  --    - price > 0
  --    - beacon id == Nothing
  = Close

  -- | Update
  -- Requirements:
  -- 1) No beacons in inputs
  -- 2) Staking credential signed or was executed.
  -- 3) All outputs to address contain proper datum.
  --    - price > 0
  --    - beacon id == Nothing
  | Update
  
  -- | Swap
  -- Requirements:
  -- 1) No reference scripts among swap inputs - this also protects the beacon.
  -- 2) All input prices used in swap are > 0
  -- 3) All outputs to address contain proper datum.
  --    - price == weighted avg (guaranteed to be positive if all inputs have positive prices)
  --    - beacon id == Nothing
  -- 4) offered asset taken * price <= given asset
  -- 5) Only offered asset leaves address
  | Swap

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''Action

-------------------------------------------------
-- Beacon Settings
-------------------------------------------------
-- | Beacon Redeemer
data BeaconRedeemer
  -- | Minting requirements:
  -- 1) Only one beacon is minted and uses the empty token name.
  -- 2) The beacon is stored in an address protected by the dapp's spending script for that trading
  --     pair.
  -- 3) The beacon is stored in an address with a staking credential.
  -- 4) The beacon must be stored with the reference scrip for dapp's spending script for that
  --    trading pair.
  -- 5) The datum of the output containing the beacon must be valid:
  --      - contains the proper beacon symbol
  = MintBeacon

  -- | Since minting is heavily controlled, the only requirent for this redeemer is that it is only
  -- used to burn. Burning is always allowed.
  | BurnBeacon

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-- | Used to generalize the beacon code.
-- This allows a separate beacon for every name.
-- To force uniqueness, it is used in an error message.
-- This is helpfull for testing the beacon logic without using the real beacon.
type AppName = BuiltinString

-------------------------------------------------
-- On-Chain Swap Script
-------------------------------------------------
mkSwapScript :: SwapConfig  -- ^ Extra parameter
             -> SwapDatum -> Action -> ScriptContext -> Bool
mkSwapScript SwapConfig{..} swapDatum action ctx@ScriptContext{scriptContextTxInfo=info} =
  case action of
    Close ->
      -- | All outputs to address must contain proper datum.
      swapOutputInfo Nothing `seq`
      -- | All beacons in inputs must be burned.
      traceIfFalse "Beacons not burned" beaconsBurned &&
      -- | Staking credential must sign or be executed.
      traceIfFalse "Staking credential did not approve: pubkey must sign or script must be executed" 
        stakingCredApproves
    Update ->
      -- | All outputs to address must contain proper datum.
      swapOutputInfo Nothing `seq`
      -- | No beacons in inputs.
      traceIfFalse "No beacons allowed in tx inputs" noBeaconInputs &&
      -- | Staking credential must sign or be executed.
      traceIfFalse "Staking credential did not approve: pubkey must sign or script must be executed"
        stakingCredApproves
    Swap ->
      -- | swapCheck checks:
      -- 1) All input prices used in swap are > 0.
      -- 2) No reference scripts among swap inputs (protects beacon).
      -- 3) All outputs to address contain proper datum.
      -- 4) offered asset taken * price <= given asset
      -- 5) Only offered asset leaves address.
      swapCheck

  where
    beaconInput :: Integer
    beaconInput = case swapBeacon swapDatum of
      Nothing -> 0
      Just beaconSym ->
        let inputs = txInfoInputs info
            foo acc TxInInfo{txInInfoResolved=TxOut{txOutValue=iVal}} =
              acc + valueOf iVal beaconSym adaToken
        in foldl' foo 0 inputs

    beaconsBurned :: Bool
    beaconsBurned = case swapBeacon swapDatum of
      Nothing -> True
      Just beaconSym ->
        Num.negate beaconInput == valueOf (txInfoMint info) beaconSym adaToken

    noBeaconInputs :: Bool
    noBeaconInputs = beaconInput == 0

    -- | Get the credential for this input.
    -- Used to check asset flux for address and ensure staking credential approves when necessary.
    inputCredentials :: Address
    inputCredentials = 
      let Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=addr}} = findOwnInput ctx
      in addr
      
    stakingCredApproves :: Bool
    !stakingCredApproves = case addressStakingCredential inputCredentials of
      -- | This is to prevent permanent locking of funds.
      -- The DEX is not meant to be used without a staking credential.
      Nothing -> True

      -- | Check if staking credential signals approval.
      Just stakeCred@(StakingHash cred) -> case cred of
        PubKeyCredential pkh -> txSignedBy info pkh
        ScriptCredential _ -> isJust $ Map.lookup stakeCred $ txInfoWdrl info
      
      Just _ -> traceError "Wrong kind of staking credential."

    parseDatum :: BuiltinString -> OutputDatum -> SwapDatum
    parseDatum err d = case d of
      (OutputDatum (Datum d')) -> case fromBuiltinData d' of
        Nothing -> traceError err
        Just p -> p
      _ -> traceError "All swap datums must be inline datums."

    validDatum :: Maybe Price -> SwapDatum -> Bool
    validDatum maybePrice datum
      | swapPrice datum <= fromInteger 0 = traceError "Datum price not > 0"
      | swapBeacon datum /= Nothing = traceError "Datum beacon not Nothing"
      | otherwise = case maybePrice of
          Nothing -> True
          Just price ->
            if swapPrice datum /= price
            then traceError "Datum price /= weighted avg"
            else True

    -- | Used to check the outputs to the address.
    -- Close and Update will pass in Nothing for Maybe Price.
    -- Swap will pass in the weighted avg for all of the inputs.
    swapOutputInfo :: Maybe Price -> Value
    swapOutputInfo maybePrice =
      let outputs = txInfoOutputs info
          foo val TxOut{txOutDatum=d
                       ,txOutValue=oVal
                       ,txOutAddress=addr
                       } =
            if addr == inputCredentials 
            then validDatum maybePrice (parseDatum "Invalid datum in output" d) `seq` val <> oVal
            else val  -- ^ It is for a different address. Ignore it.
      in foldl' foo mempty outputs

    -- | The total input value from this address.
    -- The Integer will be ignored.
    -- The price returned is the weighted avg.
    swapInputInfo :: (Value,(Integer,Price))
    swapInputInfo =
      let inputs = txInfoInputs info
          foo x@(val,(taken,wp)) TxInInfo{txInInfoResolved=TxOut{txOutAddress=addr
                                                                ,txOutDatum=d
                                                                ,txOutValue=iVal
                                                                ,txOutReferenceScript = maybeRef
                                                                }} =
            case maybeRef of
              Nothing ->
                if addr == inputCredentials
                then let  iTaken = uncurry (valueOf iVal) swapOffer
                          price = swapPrice $ parseDatum "Invalid datum in input" d
                          newTaken = taken + iTaken
                          newWeightedPrice =
                            ratio' taken newTaken * wp +
                            ratio' iTaken newTaken * price
                          newSwapValue = val <> iVal
                      in if price > fromInteger 0
                        then ( newSwapValue
                              , (newTaken,newWeightedPrice)
                              )
                        else traceError "All input prices must be > 0"
                else x -- ^ Not for this address. Skip.
              Just _ ->
                if addr == inputCredentials
                then traceError "Cannot consume reference script from swap address"
                else x
      in foldl' foo (mempty,(0,fromInteger 0)) inputs

    swapCheck :: Bool
    swapCheck =
      let -- | Input info
          -- Throws error if price in datum is < 0.
          -- Throws error if reference script among inputs.
          (scriptInputValue,(_,weightedPrice)) = swapInputInfo

          -- | Output info
          -- Throws error if output datum does not contain the weighted avg price.
          -- Throws error if output datum does not have Nothing for swapBeacon.
          scriptOutputValue = swapOutputInfo (Just weightedPrice)

          -- | Convert price if necessary
          correctedPrice
            -- | If ADA is offered, divide the weighted price by 1,000,000
            | swapOffer == (adaSymbol,adaToken) = weightedPrice * ratio' 1 1_000_000
            -- | If ADA is asked, multiply the weighted price by 1,000,000
            | swapAsk == (adaSymbol,adaToken) = weightedPrice * ratio' 1_000_000 1
            | otherwise = weightedPrice
          
          -- | Value flux
          scriptValueDiff = scriptOutputValue <> Num.negate scriptInputValue

          -- | Amounts
          askedGiven = fromInteger $ uncurry (valueOf scriptValueDiff) swapAsk
          offeredTaken = fromInteger $ Num.negate $ uncurry (valueOf scriptValueDiff) swapOffer

          -- | Assets leaving script address
          leavingAssets = flattenValue $ fst $ split scriptValueDiff -- ^ zero diff amounts removed
          isOnlyOfferedAsset [(cn,tn,_)] = (cn,tn) == swapOffer
          isOnlyOfferedAsset [] = True -- ^ Useful for testing
          isOnlyOfferedAsset _ = False
      in -- | Only the offered asset is allowed to leave the script address.
         -- When ADA is not being offered, the user is required to supply the ADA for native token utxos.
         -- This means that, when ADA is not offered, the script's ADA value can only increase.
         traceIfFalse "Only the offered asset is allowed to leave." (isOnlyOfferedAsset leavingAssets) &&

         -- | Ratio sets the maximum amount of the offered asset that can be taken.
         -- To withdraw more of the offered asset, more of the asked asset must be deposited to the script.
         -- Uses the corrected price to account for converting ADA to lovelace 
         traceIfFalse "Not enough of the asked asset given: offeredTaken * price <= askedGiven" 
           (offeredTaken * correctedPrice <= askedGiven)

data Swap
instance ValidatorTypes Swap where
  type instance RedeemerType Swap = Action
  type instance DatumType Swap = SwapDatum

swapValidator :: SwapConfig -> Validator
swapValidator swapConfig = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Swap
    ($$(PlutusTx.compile [|| mkSwapScript ||])
      `PlutusTx.applyCode` PlutusTx.liftCode swapConfig)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator

swapValidatorScript :: SwapConfig -> Script
swapValidatorScript = unValidatorScript . swapValidator

swapValidatorHash :: SwapConfig -> ValidatorHash
swapValidatorHash = Scripts.validatorHash . swapValidator

-------------------------------------------------
-- On-Chain Beacon Policy
-------------------------------------------------
-- | Every validator hash will get its own beacon.
-- The AppName is useful for testing so that you don't use the primary beacon.
-- When mistakes are made during testing, the beacon usage can be cluttered (like when you forget
-- a datum and permanently lock a beacon on-chain).
mkBeaconPolicy :: AppName -> ValidatorHash  -- ^ Extra parameters
               -> BeaconRedeemer -> ScriptContext -> Bool
mkBeaconPolicy appName dappHash r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintBeacon -> 
      -- | Only one beacon minted and uses the empty token name.
      mintCheck MintBeacon &&
      -- | Beacon goes to a valid destination. 
      -- 1) The beacon is stored in an address protected by the dapp's spending script.
      -- 2) The beacon is stored in an address with a staking credential.
      -- 3) The beacon is stored in a utxo containing the reference script for the dapp's
      --    spending script.
      -- 4) The datum of the output containing the beacon must contain the proper beacon symbol.
      destinationCheck

    BurnBeacon ->
      -- | Only used to burn.
      mintCheck BurnBeacon

  where
    beaconSym :: CurrencySymbol
    beaconSym = ownCurrencySymbol ctx

    -- | Returns only the beacons minted/burned
    beaconMint :: [(CurrencySymbol,TokenName,Integer)]
    beaconMint = case Map.lookup beaconSym $ getValue $ txInfoMint info of
      Nothing -> traceError "MintError"
      Just bs -> flattenValue $ Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintCheck :: BeaconRedeemer -> Bool
    mintCheck r' = case (r',beaconMint) of
      (MintBeacon, [(_,tn,n)]) -> 
        traceIfFalse "Only the beacon with an empty token name can be minted" (tn == adaToken) &&
        traceIfFalse "One, and only one, beacon must be minted with this redeemer." (n == 1)
      (MintBeacon, _) -> traceError "Only the beacon with an empty token name can be minted"
      (BurnBeacon, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs) 

    parseDatum :: BuiltinString -> OutputDatum -> SwapDatum
    parseDatum err d = case d of
      (OutputDatum (Datum d')) -> case fromBuiltinData d' of
        Nothing -> traceError err
        Just p -> p
      _ -> traceError "All swap datums must be inline datums."
    
    validDatum :: SwapDatum -> Bool
    validDatum datum
      | swapBeacon datum /= Just beaconSym = traceError "Datum beacon not Just beaconPolicyId"
      -- | Price can be ignored since this utxo cannot be used in a swap.
      | otherwise = True

    -- | A helper function for destinationCheck to make the code easier to reason about.
    -- This uses the appName in the error message.
    validDestination :: ValidatorHash -> PlutusApi.ScriptHash -> Bool
    validDestination spendVh@(ValidatorHash vs) refScript
      | spendVh /= dappHash = traceError ("Beacon not minted to the proper " <> appName <> " address")
      | vs /= getScriptHash refScript = 
          traceError "Beacon not stored with spending reference script for address"
      | otherwise = True

    -- | Check if the beacon is going ot a valid address with a valid datum and is stored with
    -- the proper reference script.
    destinationCheck :: Bool
    destinationCheck =
      let outputs = txInfoOutputs info
          foo acc TxOut{txOutDatum=d
                       ,txOutValue=oVal
                       ,txOutReferenceScript=maybeRef
                       ,txOutAddress=Address{addressCredential=addrCred
                                            ,addressStakingCredential=maybeStakeCred
                                            }
                       } =
            let datum = parseDatum "Invalid datum in output" d
            in if valueOf oVal beaconSym adaToken == 1
               then case (addrCred,maybeStakeCred,maybeRef) of
                (ScriptCredential vh, Just (StakingHash _),Just ss) ->
                  -- | validDestination and validDatum will both fail with traceError unless True.
                  acc && validDestination vh ss && validDatum datum
                (ScriptCredential _, _, Just _) -> 
                  traceError "Beacon not minted to a script address with a proper staking credential"
                (ScriptCredential _, _ , Nothing) ->
                  traceError "Beacon not stored with a reference script"
                (PubKeyCredential _, _, _) ->
                  traceError ("Beacon not minted to a " <> appName <> " script address")
               else acc
      in foldl' foo True outputs
                
beaconPolicy' :: AppName -> SwapConfig -> MintingPolicy
beaconPolicy' appName swapConfig = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode appName
    `PlutusTx.applyCode` PlutusTx.liftCode spendHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkBeaconPolicy x y
    spendHash = swapValidatorHash swapConfig

beaconPolicy :: SwapConfig -> MintingPolicy
beaconPolicy = beaconPolicy' "cardano-swaps-testing-abc123"

beaconScript :: SwapConfig -> Script
beaconScript = unMintingPolicyScript . beaconPolicy

beaconSymbol :: SwapConfig -> CurrencySymbol
beaconSymbol = scriptCurrencySymbol . beaconPolicy

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