{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
  , grouped
  , testTrace

    -- * Re-exports
  , E.runEmulatorM
  , E.defaultOptions
  , E.MonadEmulator
  , E.nextSlot
  , PV2.OutputDatum(..)
  , void
  , alwaysSucceedValidator
  , alwaysSucceedPolicy
  , L.CardanoAddress
  , TxOutRef
  ) where

import GHC.Generics (Generic)
import qualified Ledger.Tx.CardanoAPI as C
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Ledger as L
import qualified Ledger.Tx.CardanoAPI.Internal as LTx
import qualified Ledger.Value.CardanoAPI as LV
import qualified PlutusLedgerApi.V2 as PV2
import qualified Cardano.Node.Emulator as E
import qualified Cardano.Node.Emulator.LogMessages as E 
import qualified Cardano.Node.Emulator.Internal.Node as E hiding (queueTx,nextSlot)
import qualified Ledger.Index as Index
import qualified Ledger.AddressMap as AM
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy,alwaysSucceedValidator)
import Plutus.Script.Utils.Value (flattenValue)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (foldl',toList)
import Control.Monad (void,unless)
import Lens.Micro ((^.),(^?))
import Control.Monad.RWS.Class (get)
import Prettyprinter (pretty,vsep)
import qualified Prettyprinter as Pretty
import Test.Tasty (TestName,TestTree)
import Test.Tasty.HUnit (testCase,assertFailure)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (runRWS)

import CardanoSwaps.Utils

-------------------------------------------------
-- Core Test Framework
-------------------------------------------------
-- | Mint or burn native tokens. Can use either a reference script or a local script.
data TokenMint = TokenMint 
  { mintTokens :: [(PV2.TokenName,Integer)]
  , mintRedeemer :: PV2.Redeemer
  , mintPolicy :: L.Versioned L.MintingPolicy
  , mintReference :: Maybe L.TxOutRef
  } deriving stock (Generic,Show,Eq)

-- | Create a transaction output at the specified address with the specified value, datum,
-- and reference script.
data Output = Output
  { outputAddress :: L.CardanoAddress
  , outputValue :: LV.Value
  , outputDatum :: PV2.OutputDatum
  , outputReferenceScript :: L.ReferenceScript
  } deriving (Generic,Show,Eq)

-- | What kind of datum the input being spent has.
data InputDatum
  = InlineDatum
  | DatumHash PV2.Datum
  deriving (Generic,Show,Eq)

inputDatumToScriptDatum :: InputDatum -> C.ScriptDatum C.WitCtxTxIn
inputDatumToScriptDatum InlineDatum = C.InlineScriptDatum
inputDatumToScriptDatum (DatumHash d) = 
  C.ScriptDatumForTxIn $ LTx.toCardanoScriptData $ PV2.getDatum d

-- | A witness for a spend event.
data SpendWitness
  = SpendWithPubKey
  | SpendWithPlutusScript (L.Versioned L.Script) InputDatum L.Redeemer
  | SpendWithPlutusReference L.TxOutRef InputDatum L.Redeemer
  deriving (Generic,Show,Eq)

-- | Spend a transaction input.
data Input = Input
  { inputId :: L.TxOutRef
  , inputWitness :: SpendWitness
  } deriving (Generic,Show,Eq)

-- | A witness for a stake event.
data StakeWitness
  = StakeWithPubKey
  | StakeWithPlutusScript (L.Versioned L.Script) L.Redeemer
  | StakeWithPlutusReference L.TxOutRef L.Redeemer
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
  } deriving (Generic,Show,Eq)

emptyTxParams :: TransactionParams
emptyTxParams = TransactionParams [] [] [] [] [] [] []

transact 
  :: (E.MonadEmulator m) 
  => L.CardanoAddress 
  -- ^ The main address where change will be returned.
  -> [L.CardanoAddress] 
  -- ^ Any other addresses where inputs will come from. This includes reference inputs
  -> [L.PaymentPrivateKey] 
  -- ^ All private keys that must sign the transaction. This does not mean a plutus script will
  -- see the signature. You must include the key in the `extraKeyWitnesses` of `TransactionParams`
  -- for a plutus script to see it.
  -> TransactionParams 
  -> m LTx.CardanoTx
transact mainAddress extraAddresses privKeys TransactionParams{..} = do
  utxos <- foldl' (<>) <$> E.utxosAt mainAddress <*> mapM E.utxosAt extraAddresses
  let mintWitnessMap = C.BuildTxWith $ Map.fromList $ flip map tokens $ \TokenMint{..} -> 
        ( LV.policyId mintPolicy
        , unsafeFromRight $ 
            C.toCardanoMintWitness mintRedeemer (toVersioned <$> mintReference) (Just mintPolicy)
        )
      mintValue = flip foldMap tokens $ \TokenMint{..} -> 
        flip foldMap mintTokens $ \(tn,i) -> 
          LV.singleton (LV.policyId mintPolicy) (unsafeFromRight $ C.toCardanoAssetName tn) i
      outs = flip map outputs $ \Output{..} ->
        C.TxOut
          outputAddress
          (C.toCardanoTxOutValue outputValue)
          (unsafeFromRight $ C.toCardanoTxOutDatum outputDatum)
          outputReferenceScript
      ins = flip map inputs $ \Input{..} ->
        ( unsafeFromRight $ LTx.toCardanoTxIn inputId
        , C.BuildTxWith $ case inputWitness of
            SpendWithPubKey -> 
              C.KeyWitness C.KeyWitnessForSpending
            SpendWithPlutusScript script datum redeemer -> 
              C.ScriptWitness C.ScriptWitnessForSpending $
                C.toCardanoTxInScriptWitnessHeader 
                  script
                  (inputDatumToScriptDatum datum)
                  (C.toCardanoScriptData $ PV2.getRedeemer redeemer)
                  LTx.zeroExecutionUnits -- The autobalancer will set these.
            SpendWithPlutusReference ref datum redeemer-> 
              C.ScriptWitness C.ScriptWitnessForSpending $
                (unsafeFromRight $ C.toCardanoTxInReferenceWitnessHeader $ toVersioned ref)
                  (inputDatumToScriptDatum datum)
                  (C.toCardanoScriptData $ PV2.getRedeemer redeemer)
                  LTx.zeroExecutionUnits -- The autobalancer will set these.
        )
      wdrls = flip map withdrawals $ \Withdrawal{..} ->
        ( C.makeStakeAddress E.testnet $ case withdrawalCredential of
            PV2.ScriptCredential hash -> 
              C.StakeCredentialByScript $ unsafeFromRight $ C.toCardanoScriptHash hash
            PV2.PubKeyCredential hash -> 
              C.StakeCredentialByKey $ unsafeFromRight $ C.toCardanoStakeKeyHash hash
        , withdrawalAmount
        , C.BuildTxWith $ case withdrawalWitness of
            StakeWithPubKey -> C.KeyWitness C.KeyWitnessForStakeAddr
            StakeWithPlutusScript script redeemer-> 
              C.ScriptWitness C.ScriptWitnessForStakeAddr $
                C.toCardanoTxInScriptWitnessHeader 
                  script
                  C.NoScriptDatumForStake
                  (C.toCardanoScriptData $ PV2.getRedeemer redeemer)
                  LTx.zeroExecutionUnits -- The autobalancer will set these.
            StakeWithPlutusReference ref redeemer-> 
              C.ScriptWitness C.ScriptWitnessForStakeAddr $
                (unsafeFromRight $ C.toCardanoTxInReferenceWitnessHeader $ toVersioned ref)
                  C.NoScriptDatumForStake
                  (C.toCardanoScriptData $ PV2.getRedeemer redeemer)
                  LTx.zeroExecutionUnits -- The autobalancer will set these.
        )
      references = 
        C.TxInsReference C.BabbageEraOnwardsBabbage $ 
          map (unsafeFromRight . LTx.toCardanoTxIn) referenceInputs
      (certs,certWits) = unzip $ flip map certificates $ \Certificate{..} ->
        let stakeCred = case certificateCredential of
              PV2.ScriptCredential hash -> 
                C.StakeCredentialByScript $ unsafeFromRight $ C.toCardanoScriptHash hash
              PV2.PubKeyCredential hash -> 
                C.StakeCredentialByKey $ unsafeFromRight $ C.toCardanoStakeKeyHash hash
        in ( case certificateAction of
               Register -> 
                 C.makeStakeAddressRegistrationCertificate $
                   C.StakeAddrRegistrationPreConway
                     C.ShelleyToBabbageEraBabbage 
                     stakeCred
               UnRegister -> 
                 C.makeStakeAddressUnregistrationCertificate $
                   C.StakeAddrRegistrationPreConway
                     C.ShelleyToBabbageEraBabbage 
                     stakeCred
           , (stakeCred,) $
              case certificateWitness of
                StakeWithPubKey -> C.KeyWitness C.KeyWitnessForStakeAddr
                StakeWithPlutusScript script redeemer-> 
                  C.ScriptWitness C.ScriptWitnessForStakeAddr $
                    C.toCardanoTxInScriptWitnessHeader 
                      script
                      C.NoScriptDatumForStake
                      (C.toCardanoScriptData $ PV2.getRedeemer redeemer)
                      LTx.zeroExecutionUnits -- The autobalancer will set these.
                StakeWithPlutusReference ref redeemer-> 
                  C.ScriptWitness C.ScriptWitnessForStakeAddr $
                    (unsafeFromRight $ C.toCardanoTxInReferenceWitnessHeader $ toVersioned ref)
                      C.NoScriptDatumForStake
                      (C.toCardanoScriptData $ PV2.getRedeemer redeemer)
                      LTx.zeroExecutionUnits -- The autobalancer will set these.
           )
      tx =
        C.CardanoBuildTx $ E.emptyTxBodyContent
          { C.txMintValue = C.TxMintValue C.MaryEraOnwardsBabbage mintValue mintWitnessMap
          , C.txIns = ins
          , C.txOuts = outs
          , C.txInsReference = references
          , C.txExtraKeyWits = C.TxExtraKeyWitnesses C.AlonzoEraOnwardsBabbage $
              map (unsafeFromRight . toCardanoApiKeyHash) extraKeyWitnesses
          , C.txWithdrawals = C.TxWithdrawals C.ShelleyBasedEraBabbage wdrls
          , C.txCertificates = C.TxCertificates C.ShelleyBasedEraBabbage certs $
              C.BuildTxWith $ Map.fromList certWits
          }
  E.submitTxConfirmed utxos mainAddress privKeys tx

-------------------------------------------------
-- Basic Configs
-------------------------------------------------
-- | An always succeeding validator address without a staking credential. All reference scripts
-- are assumed to be stored here.
refScriptAddress :: L.CardanoAddress
refScriptAddress = 
  let alwaysScriptHash = scriptHash $ L.unScript $ L.getValidator alwaysSucceedValidator 
  in toCardanoApiAddress $ PV2.Address (PV2.ScriptCredential alwaysScriptHash) Nothing

testTokenSymbol :: CurrencySymbol
testTokenSymbol = CurrencySymbol 
                $ PV2.getScriptHash 
                $ scriptHash 
                $ L.unScript 
                $ L.unMintingPolicyScript alwaysSucceedPolicy

-------------------------------------------------
-- EmulatorPredicates
-------------------------------------------------
mustSucceed :: TestName -> E.EmulatorM a -> TestTree
mustSucceed testName contract =
  testCase testName $
    let (res,(st,lg)) = E.runEmulatorM E.defaultOptions contract
    in case res of
          Left err -> assertFailure $ show err
          Right _ ->  -- Just because it returned a Right does not mean everything succeeded.
            let (res1, _, _) = 
                  runRWS (runExceptT (hasNoFailedTransactions lg)) (E.params E.defaultOptions) st
            in case res1 of
                Left err -> assertFailure $ show err
                -- Right (Just msg) -> assertFailure $ unpack (E.renderLogs lg) ++ "\n" ++ msg
                Right (Just msg) -> assertFailure msg
                _ -> pure ()

mustExceedTxLimits :: TestName -> E.EmulatorM a -> TestTree
mustExceedTxLimits testName contract =
  testCase testName $
    let (res,(st,lg)) = E.runEmulatorM E.defaultOptions contract
    in case res of
          Left err -> assertFailure $ show err -- It should return Right.
          Right _ ->  -- Just because it returned a Right does not mean everything succeeded.
            let (res1, _, _) = 
                  runRWS (runExceptT (hasExceededTxLimits lg)) (E.params E.defaultOptions) st
            in case res1 of
                Left err -> assertFailure $ show err
                Right (Just msg) -> assertFailure msg
                _ -> pure ()

scriptMustFail :: TestName -> E.EmulatorM a -> TestTree
scriptMustFail testName contract =
  testCase testName $
    let (res,_) = E.runEmulatorM E.defaultOptions contract
    in case res of
          Left (E.ValidationError (_,Index.ScriptFailure _)) -> pure ()
          Left err -> assertFailure $ "Emulator terminated for other reason:" <> "\n" <> show err
          Right _ -> assertFailure "Transaction succeeded"

scriptMustFailWithError :: TestName -> Text -> E.EmulatorM a -> TestTree
scriptMustFailWithError testName errCode contract =
  testCase testName $
    let (res,_) = E.runEmulatorM E.defaultOptions contract
    in case res of
          Left err@(E.ValidationError (_,Index.ScriptFailure (L.EvaluationError errs _))) -> 
            unless (errCode `elem` errs) $ 
              assertFailure $ "Script failed for different reason:" <> "\n" <> show err
          Left err -> assertFailure $ "Transaction failed for different reason:" <> "\n" <> show err
          Right _ -> assertFailure "Transaction succeeded"

hasNoFailedTransactions :: E.EmulatorLogs -> E.EmulatorM (Maybe String)
hasNoFailedTransactions Empty = pure Nothing
hasNoFailedTransactions (l :<| lg) = case l of
  E.LogMessage _ (E.ChainEvent (E.TxnValidation Index.FailPhase1{})) -> 
    pure $ Just $ show $ pretty l
  E.LogMessage _ (E.ChainEvent (E.TxnValidation Index.FailPhase2{})) -> 
    pure $ Just $ show $ pretty l
  E.LogMessage _ (E.TxBalanceMsg E.ValidationFailed{}) -> 
    pure $ Just $ show $ pretty l
  _otherLogMsg -> hasNoFailedTransactions lg

hasExceededTxLimits :: E.EmulatorLogs -> E.EmulatorM (Maybe String)
hasExceededTxLimits Empty = pure $ Just "No transactions exceeded limits"
hasExceededTxLimits (l :<| lg) = do
  let properMsg m = m == "(MaxTxSizeUTxO" || m == "(ExUnitsTooBigUTxO"
      hasProperMsg msgs =
        if any properMsg $ T.words msgs
        then pure Nothing
        else hasExceededTxLimits lg
  case l of
    E.LogMessage _ (E.ChainEvent (E.TxnValidation (Index.FailPhase1 _ msg))) -> 
      case msg ^? L._CardanoLedgerValidationError of
        Just err -> hasProperMsg err
        Nothing -> hasExceededTxLimits lg
    E.LogMessage _ (E.ChainEvent (E.TxnValidation (Index.FailPhase2 _ msg _))) -> 
      case msg ^? L._CardanoLedgerValidationError of
        Just err -> hasProperMsg err
        Nothing -> hasExceededTxLimits lg
    E.LogMessage _ (E.TxBalanceMsg (E.ValidationFailed _ _ msg _)) -> 
      case msg ^? L._CardanoLedgerValidationError of
        Just err -> hasProperMsg err
        Nothing -> hasExceededTxLimits lg
    _otherLogMsg -> hasExceededTxLimits lg

-- not :: Maybe String -> Maybe String
-- not (Just _) = Nothing
-- not Nothing = Just ""

-- hasFailedTransaction :: EmulatorPredicate
-- hasFailedTransaction = fmap Test.Prelude.not . hasNoFailedTransactions

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toReferenceScript :: Maybe PV2.SerialisedScript -> C.ReferenceScript C.BabbageEra
toReferenceScript = C.toCardanoReferenceScript . fmap toVersionedLedgerScript

toVersionedMintingPolicy :: PV2.SerialisedScript -> L.Versioned L.MintingPolicy
toVersionedMintingPolicy = wrapVersionedLedgerScript L.MintingPolicy . toVersionedLedgerScript

toVersioned :: a -> L.Versioned a
toVersioned x = L.Versioned x L.PlutusV2

toCardanoApiAddress :: PV2.Address -> L.CardanoAddress
toCardanoApiAddress = unsafeFromRight . C.toCardanoAddressInEra E.testnet

toRedeemer :: PV2.ToData a => a -> L.Redeemer
toRedeemer = L.Redeemer . PV2.dataToBuiltinData . PV2.toData

toDatum :: PV2.ToData a => a -> L.Datum
toDatum = L.Datum . PV2.dataToBuiltinData . PV2.toData

toCardanoValue :: PV2.Value -> C.Value
toCardanoValue = unsafeFromRight . LV.toCardanoValue

utxoValue :: L.Lovelace -> PV2.Value -> C.Value
utxoValue lovelace v = LV.lovelaceToValue lovelace <> unsafeFromRight (LV.toCardanoValue v)

-- | Find the TxOutRef for the first UTxO with a specific value.
txOutRefWithValue :: (E.MonadEmulator m) => C.Value -> m TxOutRef
txOutRefWithValue value = do
  state <- get
  let xs = Map.toList $ C.unUTxO $ state ^. E.esChainState . E.index
      findTxId v ((ref,o):ys)
        | PV2.txOutValue (LTx.fromCardanoTxOutToPV2TxInfoTxOut' o) == LV.fromCardanoValue v = ref
        | otherwise = findTxId v ys
      findTxId _ _ = error "Test.Prelude.txOutRefWithValue error"
  return $ LTx.fromCardanoTxIn $ findTxId value xs

-- | Find the TxOutRef for the first UTxO with a reference script.
txOutRefWithReferenceScript :: (E.MonadEmulator m) => PV2.ScriptHash -> m TxOutRef
txOutRefWithReferenceScript hash = do
  state <- get
  let xs = Map.toList $ C.unUTxO $ state ^. E.esChainState . E.index
      findTxId h ((ref,o):ys)
        | PV2.txOutReferenceScript (LTx.fromCardanoTxOutToPV2TxInfoTxOut' o) == Just h = ref
        | otherwise = findTxId h ys
      findTxId _ _ = error "Test.Prelude.txOutRefWithValue error"
  return $ LTx.fromCardanoTxIn $ findTxId hash xs

-- | Find all TxOutRefs and their datums located at a specific address.
txOutRefsAndDatumsAtAddress 
  :: forall a m. (E.MonadEmulator m, PV2.FromData a) 
  => L.CardanoAddress 
  -> m [(TxOutRef,Maybe a)]
txOutRefsAndDatumsAtAddress addr = do
  state <- get
  let xs = Map.toList $ C.unUTxO $ state ^. E.esChainState . E.index
      addr' = L.toPlutusAddress addr
      findTxId [] = []
      findTxId ((ref,o):ys) =
        let out = LTx.fromCardanoTxOutToPV2TxInfoTxOut' o
        in if PV2.txOutAddress out == addr'
           then (LTx.fromCardanoTxIn ref, convertFromData @a $ PV2.txOutDatum out) : findTxId ys
           else findTxId ys
  return $ findTxId xs

convertFromData :: forall d . PV2.FromData d => PV2.OutputDatum -> Maybe d
convertFromData datum = case datum of
  PV2.NoOutputDatum -> Nothing
  PV2.OutputDatumHash _ -> Nothing
  PV2.OutputDatum (PV2.Datum scriptData) ->
    PV2.fromBuiltinData @d $ scriptData

toCardanoApiKeyHash :: PV2.PubKeyHash -> Either LTx.ToCardanoError (C.Hash C.PaymentKey)
toCardanoApiKeyHash (PV2.PubKeyHash bs) =
  let bsx = PV2.fromBuiltin bs
      tg = "toCardanoApiKeyHash (" <> show (B.length bsx) <> " bytes)"
   in LTx.tag tg $ LTx.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n xs = 
  let (m,ms) = splitAt n xs 
  in m : grouped n ms

testTraceLastLogs :: Int -> E.EmulatorM a -> IO ()
testTraceLastLogs n trace = 
  let (_,(_,lg)) = E.runEmulatorM E.defaultOptions trace
  in print $ vsep $ toList $ fmap pretty $ Seq.reverse $ Seq.take n $ Seq.reverse lg

testTrace :: E.EmulatorM a -> IO ()
testTrace trace = do
    let (r,(st,lg)) = E.runEmulatorM E.defaultOptions trace
    case r of
      Left err -> print err
      Right _ -> do 
        let (res1, _, _) = 
              runRWS (runExceptT (hasNoFailedTransactions lg)) (E.params E.defaultOptions) st
        case res1 of
          Left err -> print err
          Right (Just msg) -> putStrLn msg
          Right Nothing ->
            print $ vsep $ prettyBalances $ Map.toList $ AM.values $ st ^. E.esAddressMap
  where
    prettyValue :: (PV2.CurrencySymbol,PV2.TokenName,Integer) -> Pretty.Doc ann
    prettyValue ("","",i) = pretty $ show i <> " lovelace"
    prettyValue (curr,name,i) = pretty $ show i <> " " <> show curr <> "." <> show name 

    prettyBalances :: [(L.CardanoAddress,C.Value)] -> [Pretty.Doc ann]
    prettyBalances ys =
      let go sc [] = []
          go sc ((addr,val):xs) = 
            if addr == refScriptAddress then 
              vsep [ "RefAddress"
                   , Pretty.indent 4 $ Pretty.align $ vsep $ 
                      map prettyValue $ flattenValue $ LV.fromCardanoValue val
                   ] : go sc xs
            else 
              case L.toPlutusAddress addr of
                PV2.Address (PV2.PubKeyCredential _) _ -> go sc xs
                PV2.Address (PV2.ScriptCredential _) _ ->
                  vsep [ "SwapAddress" <> pretty @Int sc
                       , Pretty.indent 4 $ Pretty.align $ vsep $ 
                          map prettyValue $ flattenValue $ LV.fromCardanoValue val
                       ] : go (sc + 1) xs
      in go 1 ys
