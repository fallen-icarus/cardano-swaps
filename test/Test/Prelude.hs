{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | The test framework, implemented on top of cooked-validators. This module
-- keeps the exact API of the previous cardano-node-emulator based framework:
-- tests declaratively describe transactions with 'TransactionParams' and
-- submit them with 'transact'; the tasty predicates ('mustSucceed',
-- 'scriptMustFailWithError', ...) wrap cooked-validators' testing
-- combinators.
--
-- Differences from the old framework worth knowing about:
--
-- * cooked-validators reconstructs spending witnesses from the output it
--   recorded when a UTxO was created, so the script/datum passed in a
--   'SpendWitness' is informative only. Outputs paid to script addresses must
--   use a script known to this module (see 'knownScripts').
--
-- * A transaction's signatories are derived from the change address, the
--   owners of spent pubkey UTxOs and 'extraKeyWitnesses'; the private keys
--   passed to 'transact' are ignored. All signatories appear in the
--   transaction's required signers (visible to plutus scripts), which is a
--   superset of the old behavior where only 'extraKeyWitnesses' were visible.
--
-- * 'OutputDatumHash' now takes the full datum (the hash is computed from
--   it), because cooked-validators cannot attach a bare datum hash.
module Test.Prelude
  (
    -- * Core Test Framework
    TokenMint(..)
  , Input(..)
  , InputDatum(..)
  , SpendWitness(..)
  , StakeWitness(..)
  , Withdrawal(..)
  , Certificate(..)
  , CertificateAction(..)
  , Output(..)
  , OutputDatum(..)
  , ValidityRange(..)
  , TransactionParams(..)
  , emptyTxParams
  , transact

    -- * Basic Configs
  , refScriptAddress
  , testTokenSymbol

    -- * EmulatorPredicates
  , mustSucceed
  , mustExceedTxLimits
  , scriptMustFail
  , scriptMustFailWithError

    -- * Helper Functions
  , toReferenceScript
  , toVersionedMintingPolicy
  , toCardanoApiAddress
  , toRedeemer
  , toDatum
  , utxoValue
  , txOutRefWithReferenceScript
  , txOutRefWithValue
  , txOutRefsAndDatumsAtAddress
  , toVersioned
  , testTraceLastLogs
  , posixTimeToSlot
  , slotToPosixTime
  , toNearestMin
  , grouped
  , testTrace

    -- * Re-exports
  , MonadEmulator
  , nextSlot
  , awaitTime
  , void
  , alwaysSucceedValidator
  , alwaysSucceedPolicy
  , L.CardanoAddress
  , TxOutRef
  , C.Lovelace
  ) where

import GHC.Generics (Generic)
import GHC.IsList (toList)
import qualified Cardano.Api as C
import Cardano.Api.Ledger (Coin(..))
import qualified Ledger as L
import qualified Ledger.Index as Index
import qualified Ledger.Tx.CardanoAPI.Internal as LTx
import qualified Ledger.Value.CardanoAPI as LV
import qualified PlutusLedgerApi.V1.Interval as Interval
import PlutusLedgerApi.V1.Scripts (ScriptError(EvaluationError))
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified Cardano.Node.Emulator.Internal.Node as E
import Plutus.Script.Utils.V2.Generators
  (alwaysSucceedPolicy,alwaysSucceedValidator
  ,alwaysSucceedPolicyVersioned,alwaysSucceedValidatorVersioned)
import qualified Plutus.Script.Utils.Scripts as PSU
import qualified Plutus.Script.Utils.Value as PSU (ada)

import qualified Cooked.InitialDistribution as CK
import qualified Cooked.MockChain.BlockChain as CK
import qualified Cooked.MockChain.Direct as CK
import qualified Cooked.MockChain.Staged as CK
import qualified Cooked.MockChain.Testing as CK
import qualified Cooked.Pretty as CK
import qualified Cooked.Skeleton as CK
import qualified Cooked.Wallet as CK
import qualified Plutus.Script.Utils.Address as PSU

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ((&&&))
import Data.Foldable (fold)
import Data.List (find,nub,sortOn,(\\))
import Data.Maybe (catMaybes,fromMaybe,listToMaybe)
import Data.Ord (Down(Down))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default (def)
import Control.Monad (void,forM)
import Data.String (IsString(..))
import Test.Tasty (TestName,TestTree)

import qualified PlutusTx.Builtins as Builtins

import CardanoSwaps.Utils hiding (posixTimeToSlot)

-- | The always succeeding V2 scripts used for the reference script address
-- and the test token policy live at addresses that 'transact' must be able to
-- pay to, so they are part of 'knownScripts' below.
import qualified CardanoSwaps.OneWaySwap as OneWay
import qualified CardanoSwaps.TwoWaySwap as TwoWay

-- | `CurrencySymbol` lost its `IsString` instance in newer plutus versions.
-- The old instance treated the string as raw UTF-8 bytes, which is the
-- behavior the test suite relies on. (`TokenName` gets an equivalent orphan
-- instance from cooked-validators.)
instance IsString PV2.CurrencySymbol where
  fromString = PV2.CurrencySymbol . Builtins.encodeUtf8 . fromString

-------------------------------------------------
-- The test monad
-------------------------------------------------
-- | The constraint the old framework used for test traces. It is now a synonym
-- for cooked's 'CK.MonadBlockChain'. (ConstraintKinds comes from the test
-- suite's default-extensions.)
type MonadEmulator m = CK.MonadBlockChain m

-------------------------------------------------
-- Core Test Framework
-------------------------------------------------
-- | Mint or burn native tokens. Can use either a reference script or a local script.
data TokenMint = TokenMint
  { mintTokens :: [(PV2.TokenName,Integer)]
  , mintRedeemer :: PV2.Redeemer
  , mintPolicy :: L.Versioned L.MintingPolicy
  , mintReference :: Maybe TxOutRef
  } deriving stock (Generic,Show,Eq)

-- | What datum a transaction output carries. 'OutputDatumHash' stores only
-- the hash of the given datum in the output.
data OutputDatum
  = NoOutputDatum
  | OutputDatum L.Datum
  | OutputDatumHash L.Datum
  deriving (Generic,Show,Eq)

-- | Create a transaction output at the specified address with the specified value, datum,
-- and reference script.
data Output = Output
  { outputAddress :: L.CardanoAddress
  , outputValue :: LV.Value
  , outputDatum :: OutputDatum
  , outputReferenceScript :: Maybe (L.Versioned L.Script)
  } deriving (Generic,Show,Eq)

-- | What kind of datum the input being spent has. This is informative only:
-- cooked-validators reconstructs the datum witness from the UTxO itself.
data InputDatum
  = InlineDatum
  | DatumHash PV2.Datum
  deriving (Generic,Show,Eq)

-- | A witness for a spend event. The script (or reference to it) is
-- informative for plutus scripts: the witness is reconstructed from the UTxO
-- being spent, with the reference input attached when one is given here.
data SpendWitness
  = SpendWithPubKey
  | SpendWithPlutusScript (L.Versioned L.Script) InputDatum L.Redeemer
  | SpendWithPlutusReference TxOutRef InputDatum L.Redeemer
  deriving (Generic,Show,Eq)

-- | Spend a transaction input.
data Input = Input
  { inputId :: TxOutRef
  , inputWitness :: SpendWitness
  } deriving (Generic,Show,Eq)

-- | A witness for a stake event.
data StakeWitness
  = StakeWithPubKey
  | StakeWithPlutusScript (L.Versioned L.Script) L.Redeemer
  | StakeWithPlutusReference TxOutRef L.Redeemer
  deriving (Generic,Show,Eq)

-- | Withdrawals from a reward address.
data Withdrawal = Withdrawal
  { withdrawalCredential :: PV2.Credential
  , withdrawalAmount :: C.Lovelace
  , withdrawalWitness :: StakeWitness
  } deriving (Generic,Show,Eq)

data CertificateAction
  = Register
  | UnRegister
  deriving (Generic,Show,Eq)

data Certificate = Certificate
  { certificateCredential :: PV2.Credential
  , certificateWitness :: StakeWitness
  , certificateAction :: CertificateAction
  } deriving (Generic,Show,Eq)

data ValidityRange = ValidityRange
  { validityRangeLowerBound :: Maybe L.Slot
  , validityRangeUpperBound :: Maybe L.Slot
  } deriving (Generic,Show,Eq)

-- | Used to create a transaction with the specified constraints.
data TransactionParams = TransactionParams
  { tokens :: [TokenMint]
  , inputs :: [Input]
  , outputs :: [Output]
  , referenceInputs :: [TxOutRef]
  , extraKeyWitnesses :: [PV2.PubKeyHash]
  -- ^ In order for a plutus script to see a pubkey, it must be present in this list.
  , withdrawals :: [Withdrawal]
  , certificates :: [Certificate]
  , validityRange :: ValidityRange
  } deriving (Generic,Show,Eq)

emptyTxParams :: TransactionParams
emptyTxParams = TransactionParams [] [] [] [] [] [] [] (ValidityRange Nothing Nothing)

transact
  :: (MonadEmulator m)
  => L.CardanoAddress
  -- ^ The main address where change will be returned.
  -> [L.CardanoAddress]
  -- ^ Any other addresses where inputs will come from. Unused: cooked knows
  -- the whole UTxO set.
  -> [L.PaymentPrivateKey]
  -- ^ Unused: signatories are derived from the main address, the owners of
  -- spent pubkey UTxOs, and `extraKeyWitnesses`.
  -> TransactionParams
  -> m L.CardanoTx
transact mainAddress _extraAddresses _privKeys TransactionParams{..} = do
  let mainWallet = addressToWallet mainAddress

  -- Wallets owning explicitly spent pubkey UTxOs must sign.
  inputWallets <- fmap catMaybes $ forM inputs $ \Input{inputId,inputWitness} ->
    case inputWitness of
      SpendWithPubKey -> do
        out <- CK.txSkelOutByRef (toLedgerTxOutRef inputId)
        case CK.txSkelOutOwner out of
          CK.UserPubKey pkh -> pure $ CK.walletPKHashToWallet $ PSU.toPubKeyHash pkh
          _ -> pure Nothing
      _ -> pure Nothing

  let sigWallets = nub $ mainWallet : inputWallets
      sigPkhs = map PSU.toPubKeyHash sigWallets
      extraSigs =
        [ maybe (CK.signatoryPubKey pkh) CK.signatoryWallet (CK.walletPKHashToWallet pkh)
        | pkh <- nub extraKeyWitnesses
        , pkh `notElem` sigPkhs
        ]

  wdrls <- mapM toCookedWithdrawal withdrawals
  certs <- mapM toCookedCertificate certificates

  -- Upfront coin selection, mirroring the old framework's balancer: cover
  -- whatever the outputs and mint/burn need beyond the explicit inputs, with
  -- some headroom for fees. cooked-validators' own balancing (which then only
  -- has to cover the fee) considers each candidate UTxO at most once, so
  -- without this a transaction needing native tokens plus more ADA than the
  -- token UTxO carries would not balance.
  explicitInputValues <- forM inputs $ \Input{inputId} ->
    unsafeFromRight . LV.toCardanoValue . CK.txSkelOutValue
      <$> CK.txSkelOutByRef (toLedgerTxOutRef inputId)
  ownUtxos <- CK.utxosAt $ L.toPlutusAddress mainAddress
  let mintValue = unsafeFromRight $ LV.toCardanoValue $
        flip foldMap tokens $ \TokenMint{..} ->
          foldMap (uncurry $ PV2.singleton $ PSU.toCurrencySymbol mintPolicy) mintTokens
      feeHeadroom = LV.lovelaceToValue 5_000_000
      have = mintValue <> fold explicitInputValues
      want = feeHeadroom <> foldMap outputValue outputs
      (deficit, _surplus) = LV.split $ have <> C.negateValue want
      spentRefs = map inputId inputs
      selectionPool =
        [ (fromLedgerTxOutRef ref, unsafeFromRight $ LV.toCardanoValue $ CK.txSkelOutValue out)
        | (ref, out) <- ownUtxos
        , isOnlyValueOutput out
        , fromLedgerTxOutRef ref `notElem` spentRefs
        ]
  selectedIns <-
    if LV.isZero deficit
      then pure []
      else either (fail . show) (pure . fst) $ selectCoin selectionPool deficit

  CK.validateTxSkel $ CK.txSkelTemplate
    { CK.txSkelOpts = def
        -- The old balancer always created a separate change output; merging
        -- the change into an existing output would break the value-based
        -- UTxO lookups the tests rely on.
        { CK.txSkelOptBalanceOutputPolicy = CK.DontAdjustExistingOutput
        }
    , CK.txSkelSignatories = map CK.signatoryWallet sigWallets <> extraSigs
    , CK.txSkelMints = CK.txSkelMintsFromList $ map toCookedMint tokens
    , CK.txSkelIns = Map.fromList $
        map toCookedInput inputs
          <> map ((,CK.emptyTxSkelRedeemerNoAutoFill) . toLedgerTxOutRef) selectedIns
    , CK.txSkelInsReference = Set.fromList $ map toLedgerTxOutRef referenceInputs
    , CK.txSkelOuts = map toCookedOutput outputs
    , CK.txSkelWithdrawals = CK.txSkelWithdrawalsFromList wdrls
    , CK.txSkelCertificates = certs
    , CK.txSkelValidityRange = toSlotRange validityRange
    }

-------------------------------------------------
-- Translating TransactionParams to a TxSkel
-------------------------------------------------
-- | All scripts that tests pay to, keyed by their hash. Outputs to script
-- addresses must use one of these so that cooked-validators can reconstruct
-- the witness when the output is later spent.
knownScripts :: Map.Map PV2.ScriptHash (L.Versioned L.Script)
knownScripts = Map.fromList
  [ (scriptHash OneWay.beaconScript, toVersionedLedgerScript OneWay.beaconScript)
  , (scriptHash OneWay.swapScript, toVersionedLedgerScript OneWay.swapScript)
  , (scriptHash TwoWay.beaconScript, toVersionedLedgerScript TwoWay.beaconScript)
  , (scriptHash TwoWay.swapScript, toVersionedLedgerScript TwoWay.swapScript)
  , ( PSU.toScriptHash alwaysSucceedValidatorVersioned
    , PSU.getValidator <$> alwaysSucceedValidatorVersioned
    )
  ]

resolveScript :: PV2.ScriptHash -> L.Versioned L.Script
resolveScript hash =
  fromMaybe (error $ "Test.Prelude.resolveScript: unknown script " <> show hash) $
    Map.lookup hash knownScripts

addressToWallet :: L.CardanoAddress -> CK.Wallet
addressToWallet addr = case L.toPlutusAddress addr of
  PV2.Address (PV2.PubKeyCredential pkh) _ ->
    fromMaybe (error $ "Test.Prelude.addressToWallet: unknown wallet " <> show pkh) $
      CK.walletPKHashToWallet pkh
  _ -> error "Test.Prelude.addressToWallet: not a pubkey address"

mkRedeemer :: L.Redeemer -> Maybe TxOutRef -> CK.TxSkelRedeemer
mkRedeemer (L.Redeemer red) ref =
  CK.TxSkelRedeemer red (toLedgerTxOutRef <$> ref) False

toCookedMint :: TokenMint -> CK.Mint
toCookedMint TokenMint{..} =
  CK.Mint
    (CK.UserRedeemedScript
      (PSU.getMintingPolicy <$> mintPolicy)
      (mkRedeemer mintRedeemer mintReference))
    mintTokens

toCookedInput :: Input -> (PV3.TxOutRef, CK.TxSkelRedeemer)
toCookedInput Input{..} = (,) (toLedgerTxOutRef inputId) $
  case inputWitness of
    SpendWithPubKey -> CK.emptyTxSkelRedeemerNoAutoFill
    SpendWithPlutusScript _ _ red -> mkRedeemer red Nothing
    SpendWithPlutusReference ref _ red -> mkRedeemer red (Just ref)

toCookedOutput :: Output -> CK.TxSkelOut
toCookedOutput Output{..} =
  let PV2.Address paymentCred stakingCred = L.toPlutusAddress outputAddress
      owner = case paymentCred of
        PV2.PubKeyCredential pkh -> CK.UserPubKey pkh
        PV2.ScriptCredential hash -> CK.UserScript $ resolveScript hash
  in CK.TxSkelOut
       { CK.txSkelOutOwner = owner
       , CK.txSkelOutStakingCredential = stakingCred
       , CK.txSkelOutDatum = case outputDatum of
           NoOutputDatum -> CK.NoTxSkelOutDatum
           OutputDatum (L.Datum d) -> CK.SomeTxSkelOutDatum d CK.Inline
           OutputDatumHash (L.Datum d) -> CK.SomeTxSkelOutDatum d (CK.Hashed CK.NotResolved)
       , CK.txSkelOutValue = LV.fromCardanoValue outputValue
         -- Values are used exactly as given, like in the old framework.
       , CK.txSkelOutValueAutoAdjust = False
       , CK.txSkelOutReferenceScript = outputReferenceScript
       }

-- | Stake witnesses referencing a script UTxO must resolve the script body
-- from the reference script it carries.
toCookedStakeUser
  :: (MonadEmulator m)
  => PV2.Credential -> StakeWitness -> m (CK.User CK.IsEither CK.Redemption)
toCookedStakeUser cred witness = case witness of
  StakeWithPubKey -> case cred of
    PV2.PubKeyCredential pkh -> pure $ CK.UserPubKey pkh
    PV2.ScriptCredential _ ->
      error "Test.Prelude: StakeWithPubKey used with a script credential"
  StakeWithPlutusScript script red ->
    pure $ CK.UserRedeemedScript script (mkRedeemer red Nothing)
  StakeWithPlutusReference ref red -> do
    out <- CK.txSkelOutByRef (toLedgerTxOutRef ref)
    case CK.txSkelOutReferenceScript out of
      Just script -> pure $ CK.UserRedeemedScript script (mkRedeemer red (Just ref))
      Nothing -> error "Test.Prelude: stake reference UTxO has no reference script"

toCookedWithdrawal :: (MonadEmulator m) => Withdrawal -> m CK.Withdrawal
toCookedWithdrawal Withdrawal{..} = do
  user <- toCookedStakeUser withdrawalCredential withdrawalWitness
  let Coin lovelace = withdrawalAmount
  pure $ CK.Withdrawal user (Just $ PV3.Lovelace lovelace)

toCookedCertificate :: (MonadEmulator m) => Certificate -> m CK.TxSkelCertificate
toCookedCertificate Certificate{..} = do
  let action = case certificateAction of
        Register -> CK.StakingRegister
        UnRegister -> CK.StakingUnRegister
  user <- toCookedStakeUser certificateCredential certificateWitness
  pure $ CK.TxSkelCertificate user action

-- | Whether a UTxO carries nothing but a value, making it usable for coin
-- selection (mirrors cooked's own balancing candidates).
isOnlyValueOutput :: CK.TxSkelOut -> Bool
isOnlyValueOutput out =
  case (CK.txSkelOutDatum out, CK.txSkelOutStakingCredential out, CK.txSkelOutReferenceScript out) of
    (CK.NoTxSkelOutDatum, Nothing, Nothing) -> True
    _ -> False

-------------------------------------------------
-- Coin selection (from the old framework's balancer)
-------------------------------------------------
-- | Not enough wallet outputs available to balance a transaction.
data BalancingError
  = InsufficientFunds {total :: C.Value, expected :: C.Value}
  deriving stock (Show, Eq)

-- | Given a set of @a@s with coin values, and a target value, select a number
-- of @a@ such that their total value is greater than or equal to the target.
selectCoin
  :: (Eq a)
  => [(a, C.Value)]
  -- ^ Possible inputs to choose from
  -> C.Value
  -- ^ The target value
  -> Either BalancingError ([a], C.Value)
  -- ^ The chosen inputs and the change
selectCoin fnds vl =
  let
    total = foldMap snd fnds
    err = Left $ InsufficientFunds total vl
   in
    -- Values are in a partial order: what we want to check is that the
    -- total available funds are bigger than (or equal to) the required value.
    if not (total `LV.valueGeq` vl)
      then err
      else -- Select inputs per asset class, sorting so we do Ada last.
      -- We want to do the non-Ada asset classes first, because utxo's often contain
      -- extra Ada because of fees or minAda constraints. So when we are done with the
      -- non-Ada asset classes we probably already have picked some Ada too.

        let (usedFinal, remainderFinal) = foldl step ([], vl) (sortOn Down $ toList vl)
            step (used, remainder) (assetId, _) =
              let (used', remainder') = selectCoinSingle assetId (fnds \\ used) remainder
               in (used <> used', remainder')
         in pure (map fst usedFinal, C.negateValue remainderFinal)

selectCoinSingle
  :: C.AssetId
  -> [(a, C.Value)]
  -- ^ Possible inputs to choose from
  -> C.Value
  -- ^ The target value
  -> ([(a, C.Value)], C.Value)
  -- ^ The chosen inputs and the remainder
selectCoinSingle assetId fnds' vl =
  let
    pick v = C.selectAsset v assetId
    -- We only want the values that contain the given asset class,
    -- and want the single currency values first,
    -- so that we're picking inputs that contain *only* the given asset class when possible.
    -- That being equal we want the input with the largest amount of the given asset class,
    -- to reduce the amount of inputs required. (Particularly useful to prevent hitting MaxCollateralInputs)
    fnds =
      sortOn (length . toList . snd &&& Down . pick . snd) $ filter (\(_, v) -> pick v > 0) fnds'
    -- Given the funds of a wallet, we take just enough from
    -- the target value such that the asset class value of the remainder is <= 0.
    fundsWithRemainder = zip fnds (drop 1 $ scanl (\l r -> l <> C.negateValue r) vl $ fmap snd fnds)
    fundsToSpend = takeUntil (\(_, v) -> pick v <= 0) fundsWithRemainder
    remainder = maybe vl snd $ listToMaybe $ reverse fundsToSpend
   in
    (fst <$> fundsToSpend, remainder)

-- | Take elements from a list until the predicate is satisfied.
-- 'takeUntil' @p@ includes the first element for which @p@ is true
-- (unlike @takeWhile (not . p)@).
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs

-- | The old framework's bounds behaved like cardano-api's: inclusive lower
-- bound, exclusive upper bound.
toSlotRange :: ValidityRange -> L.SlotRange
toSlotRange (ValidityRange mLower mUpper) =
  Interval.Interval
    (Interval.LowerBound (maybe Interval.NegInf Interval.Finite mLower) True)
    (Interval.UpperBound (maybe Interval.PosInf Interval.Finite mUpper) False)

-------------------------------------------------
-- Basic Configs
-------------------------------------------------
-- | An always succeeding validator address without a staking credential. All reference scripts
-- are assumed to be stored here.
refScriptAddress :: L.CardanoAddress
refScriptAddress =
  let alwaysScriptHash = PSU.toScriptHash alwaysSucceedValidatorVersioned
  in toCardanoApiAddress $ PV2.Address (PV2.ScriptCredential alwaysScriptHash) Nothing

testTokenSymbol :: CurrencySymbol
testTokenSymbol = PSU.toCurrencySymbol alwaysSucceedPolicyVersioned

-- | Every known mock wallet starts with 100M ADA in a single UTxO, matching
-- the old framework's initial distribution.
initialDistribution :: CK.InitialDistribution
initialDistribution =
  CK.distributionFromList $ map (, [PSU.ada 100_000_000]) CK.knownWallets

-------------------------------------------------
-- EmulatorPredicates
-------------------------------------------------
mustSucceed :: Show a => TestName -> CK.StagedMockChain a -> TestTree
mustSucceed testName contract =
  CK.testCooked testName $
    CK.mustSucceedTest contract `CK.withInitDist` initialDistribution

mustExceedTxLimits :: Show a => TestName -> CK.StagedMockChain a -> TestTree
mustExceedTxLimits testName contract =
  CK.testCooked testName $
    CK.mustFailTest contract
      `CK.withInitDist` initialDistribution
      `CK.withErrorProp` \err -> case err of
        CK.MCEValidationError L.Phase1 (Index.CardanoLedgerValidationError msg)
          | any (`T.isInfixOf` msg) ["MaxTxSizeUTxO","ExUnitsTooBigUTxO"] ->
              CK.testSuccess
        _ -> CK.testFailureMsg $
              "Transaction did not exceed limits:" <> "\n" <> show err

scriptMustFail :: Show a => TestName -> CK.StagedMockChain a -> TestTree
scriptMustFail testName contract =
  CK.testCooked testName $
    CK.mustFailTest contract
      `CK.withInitDist` initialDistribution
      `CK.withErrorProp` \err -> case err of
        CK.MCEValidationError _ (Index.ScriptFailure _) -> CK.testSuccess
        _ -> CK.testFailureMsg $
              "Emulator terminated for other reason:" <> "\n" <> show err

scriptMustFailWithError :: Show a => TestName -> Text -> CK.StagedMockChain a -> TestTree
scriptMustFailWithError testName errCode contract =
  CK.testCooked testName $
    CK.mustFailTest contract
      `CK.withInitDist` initialDistribution
      `CK.withErrorProp` \err -> case err of
        CK.MCEValidationError _ (Index.ScriptFailure (EvaluationError errs _))
          | errCode `elem` errs -> CK.testSuccess
          | otherwise -> CK.testFailureMsg $
              "Script failed for different reason:" <> "\n" <> show err
        _ -> CK.testFailureMsg $
              "Transaction failed for different reason:" <> "\n" <> show err

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Reference scripts stored on-chain are always the aiken PlutusV3 scripts.
toReferenceScript :: Maybe PV2.SerialisedScript -> Maybe (L.Versioned L.Script)
toReferenceScript = fmap toVersionedLedgerScript

-- | Convert the PlutusV2 `TxOutRef` used throughout the test framework to the
-- PlutusV3 `TxOutRef` cooked-validators uses.
toLedgerTxOutRef :: TxOutRef -> PV3.TxOutRef
toLedgerTxOutRef (TxOutRef (TxId h) ix) = PV3.TxOutRef (PV3.TxId h) ix

fromLedgerTxOutRef :: PV3.TxOutRef -> TxOutRef
fromLedgerTxOutRef (PV3.TxOutRef (PV3.TxId h) ix) = TxOutRef (TxId h) ix

toVersionedMintingPolicy :: PV2.SerialisedScript -> L.Versioned L.MintingPolicy
toVersionedMintingPolicy = wrapVersionedLedgerScript L.MintingPolicy . toVersionedLedgerScript

toVersioned :: a -> L.Versioned a
toVersioned x = L.Versioned x L.PlutusV2

toCardanoApiAddress :: PV2.Address -> L.CardanoAddress
toCardanoApiAddress = unsafeFromRight . LTx.toCardanoAddressInEra E.testnet

toRedeemer :: PV2.ToData a => a -> L.Redeemer
toRedeemer = L.Redeemer . PV2.dataToBuiltinData . PV2.toData

toDatum :: PV2.ToData a => a -> L.Datum
toDatum = L.Datum . PV2.dataToBuiltinData . PV2.toData

utxoValue :: C.Lovelace -> PV2.Value -> C.Value
utxoValue lovelace v = LV.lovelaceToValue lovelace <> unsafeFromRight (LV.toCardanoValue v)

-- | Find the TxOutRef for the first UTxO with a specific value.
txOutRefWithValue :: (MonadEmulator m) => C.Value -> m TxOutRef
txOutRefWithValue value = do
  utxos <- CK.allUtxos
  let target = LV.fromCardanoValue value
  case find (\(_,out) -> CK.txSkelOutValue out == target) utxos of
    Just (ref,_) -> return $ fromLedgerTxOutRef ref
    Nothing -> error "Test.Prelude.txOutRefWithValue error"

-- | Find the TxOutRef for the first UTxO with a reference script.
txOutRefWithReferenceScript :: (MonadEmulator m) => PV2.ScriptHash -> m TxOutRef
txOutRefWithReferenceScript hash = do
  utxos <- CK.allUtxos
  let hasScript (_,out) =
        (PSU.toScriptHash <$> CK.txSkelOutReferenceScript out) == Just hash
  case find hasScript utxos of
    Just (ref,_) -> return $ fromLedgerTxOutRef ref
    Nothing -> error "Test.Prelude.txOutRefWithReferenceScript error"

-- | Find all TxOutRefs and their datums located at a specific address.
txOutRefsAndDatumsAtAddress
  :: forall a m. (MonadEmulator m, PV2.FromData a)
  => L.CardanoAddress
  -> m [(TxOutRef,Maybe a)]
txOutRefsAndDatumsAtAddress addr = do
  utxos <- CK.utxosAt $ L.toPlutusAddress addr
  return $ flip map utxos $ \(ref,out) ->
    ( fromLedgerTxOutRef ref
    , case CK.txSkelOutDatum out of
        CK.NoTxSkelOutDatum -> Nothing
        CK.SomeTxSkelOutDatum d _ -> PV2.fromBuiltinData @a $ PV2.toBuiltinData d
    )

nextSlot :: (MonadEmulator m) => m ()
nextSlot = void $ CK.waitNSlots (1 :: Integer)

awaitTime :: (MonadEmulator m) => POSIXTime -> m ()
awaitTime = void . CK.awaitSlot . posixTimeToSlot

posixTimeToSlot :: POSIXTime -> L.Slot
posixTimeToSlot = E.posixTimeToEnclosingSlot def

slotToPosixTime :: L.Slot -> POSIXTime
slotToPosixTime = E.slotToBeginPOSIXTime def

toNearestMin :: POSIXTime -> POSIXTime
toNearestMin time =
  let remainder = time `mod` 60_000
  in if remainder >= 30_000
     then time + (60_000 - remainder)
     else time - remainder

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs =
  let (m,ms) = splitAt n xs
  in m : grouped n ms

-- | Run a trace and pretty-print the outcome. Purely a development helper.
testTrace :: Show a => CK.StagedMockChain a -> IO ()
testTrace trace =
  mapM_ (putStrLn . CK.renderString (CK.prettyCookedOpt def)) $
    CK.interpretAndRunWith (CK.runMockChainTFromInitDist initialDistribution) trace

-- | Kept for API compatibility; prints the full pretty-printed run.
testTraceLastLogs :: Show a => Int -> CK.StagedMockChain a -> IO ()
testTraceLastLogs _ = testTrace
