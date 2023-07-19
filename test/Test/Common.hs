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

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Test.Common where

import qualified Data.Map as Map
import Control.Lens hiding (from,index,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash,Value,lovelaceValueOf,from,validatorHash)
import Ledger.Tx.Constraints as Constraints
import qualified Ledger.Tx.Constraints.TxConstraints as Constraints
import Ledger.Tx.Constraints.TxConstraints (TxOutDatum(..),mustMintCurrencyWithRedeemerAndReference)
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Plutus.Script.Utils.Value
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Cardano.Api.Shelley (ProtocolParameters (..))
import qualified Cardano.Api as C
import Cardano.Api hiding (TxOutDatum(..),TxOutDatumInline,TxOutDatumHash,Address,TxId,Value)
import Cardano.Node.Emulator.Params
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Scripts
import Data.List (zipWith3,zipWith4)
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)

import CardanoSwaps

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
txOutRefWithValue :: Value -> EmulatorTrace TxOutRef
txOutRefWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((ref,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = ref
        | otherwise = findTxId v ys
  return $ findTxId value' xs

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
  return $ findTxId value' dHash xs

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

instance ToJSON AssetConfig
instance FromJSON AssetConfig

instance ToJSON SwapDatum
instance FromJSON SwapDatum

instance ToJSON SwapRedeemer
instance FromJSON SwapRedeemer

instance ToJSON BeaconRedeemer
instance FromJSON BeaconRedeemer

instance ToJSON DappScripts
instance FromJSON DappScripts

-------------------------------------------------
-- Configs
-------------------------------------------------
minUTxOSpendRef :: Integer
minUTxOSpendRef = 26_000_000

minUTxOMintRef :: Integer
minUTxOMintRef = 18_000_000

testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

testToken3 :: (CurrencySymbol,TokenName)
testToken3 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken3")

testToken4 :: (CurrencySymbol,TokenName)
testToken4 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken4")

testToken5 :: (CurrencySymbol,TokenName)
testToken5 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken5")

testToken6 :: (CurrencySymbol,TokenName)
testToken6 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken6")

testToken7 :: (CurrencySymbol,TokenName)
testToken7 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken7")

testToken8 :: (CurrencySymbol,TokenName)
testToken8 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken8")

testToken9 :: (CurrencySymbol,TokenName)
testToken9 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken9")

testToken10 :: (CurrencySymbol,TokenName)
testToken10 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken10")

testToken11 :: (CurrencySymbol,TokenName)
testToken11 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken11")

testToken12 :: (CurrencySymbol,TokenName)
testToken12 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken12")

testToken13 :: (CurrencySymbol,TokenName)
testToken13 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken13")

testToken14 :: (CurrencySymbol,TokenName)
testToken14 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken14")

testToken15 :: (CurrencySymbol,TokenName)
testToken15 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken15")

testToken16 :: (CurrencySymbol,TokenName)
testToken16 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken16")

testToken17 :: (CurrencySymbol,TokenName)
testToken17 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken17")

testToken18 :: (CurrencySymbol,TokenName)
testToken18 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken18")

testToken19 :: (CurrencySymbol,TokenName)
testToken19 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken19")

testToken20 :: (CurrencySymbol,TokenName)
testToken20 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken20")

testToken21 :: (CurrencySymbol,TokenName)
testToken21 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken21")

testToken22 :: (CurrencySymbol,TokenName)
testToken22 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken22")

testToken23 :: (CurrencySymbol,TokenName)
testToken23 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken23")

testToken24 :: (CurrencySymbol,TokenName)
testToken24 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken24")

testToken25 :: (CurrencySymbol,TokenName)
testToken25 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken25")

testToken26 :: (CurrencySymbol,TokenName)
testToken26 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken26")

testToken27 :: (CurrencySymbol,TokenName)
testToken27 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken27")

testToken28 :: (CurrencySymbol,TokenName)
testToken28 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken28")

testToken29 :: (CurrencySymbol,TokenName)
testToken29 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken29")

testToken30 :: (CurrencySymbol,TokenName)
testToken30 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken30")

testToken31 :: (CurrencySymbol,TokenName)
testToken31 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken31")

testToken32 :: (CurrencySymbol,TokenName)
testToken32 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken32")

testToken33 :: (CurrencySymbol,TokenName)
testToken33 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken33")

testToken34 :: (CurrencySymbol,TokenName)
testToken34 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken34")

testToken35 :: (CurrencySymbol,TokenName)
testToken35 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken35")

testToken36 :: (CurrencySymbol,TokenName)
testToken36 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken36")

testToken37 :: (CurrencySymbol,TokenName)
testToken37 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken37")

testToken38 :: (CurrencySymbol,TokenName)
testToken38 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken38")

testToken39 :: (CurrencySymbol,TokenName)
testToken39 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken39")

testToken40 :: (CurrencySymbol,TokenName)
testToken40 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken40")

testToken41 :: (CurrencySymbol,TokenName)
testToken41 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken41")

testToken42 :: (CurrencySymbol,TokenName)
testToken42 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken42")

testToken43 :: (CurrencySymbol,TokenName)
testToken43 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken43")

testToken44 :: (CurrencySymbol,TokenName)
testToken44 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken44")

testToken45 :: (CurrencySymbol,TokenName)
testToken45 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken45")

testToken46 :: (CurrencySymbol,TokenName)
testToken46 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken46")

testToken47 :: (CurrencySymbol,TokenName)
testToken47 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken47")

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: C.Value
    user1 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000
         <> (uncurry singleton testToken21) 1000
         <> (uncurry singleton testToken22) 1000
         <> (uncurry singleton testToken23) 1000
         <> (uncurry singleton testToken24) 1000
         <> (uncurry singleton testToken25) 1000
         <> (uncurry singleton testToken26) 1000
         <> (uncurry singleton testToken27) 1000
         <> (uncurry singleton testToken28) 1000
         <> (uncurry singleton testToken29) 1000
         <> (uncurry singleton testToken30) 1000
         <> (uncurry singleton testToken31) 1000
         <> (uncurry singleton testToken32) 1000
         <> (uncurry singleton testToken33) 1000
         <> (uncurry singleton testToken34) 1000
         <> (uncurry singleton testToken35) 1000
         <> (uncurry singleton testToken36) 1000
         <> (uncurry singleton testToken37) 1000
         <> (uncurry singleton testToken38) 1000
         <> (uncurry singleton testToken39) 1000
         <> (uncurry singleton testToken40) 1000

    user2 :: C.Value
    user2 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
    
    user3 :: C.Value
    user3 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user4 :: C.Value
    user4 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user5 :: C.Value
    user5 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    user6 :: C.Value
    user6 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    user7 :: C.Value
    user7 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user8 :: C.Value
    user8 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user9 :: C.Value
    user9 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    user10 :: C.Value
    user10 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    wallets :: [(Wallet,C.Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      , (knownWallet 7, user7)
      , (knownWallet 8, user8)
      , (knownWallet 9, user9)
      , (knownWallet 10, user10)
      ]

benchConfig :: EmulatorConfig
benchConfig = emConfig & params .~ params'
  where 
    params' :: Params
    params' = def{emulatorPParams = pParams'}

    pParams' :: PParams
    pParams' = pParamsFromProtocolParams protoParams

    protoParams :: ProtocolParameters
    protoParams = def{ protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
                                                                        ,executionMemory = 7000000})
                     , protocolParamMaxTxSize = 7300
                     }

-------------------------------------------------
-- Params
-------------------------------------------------
data CreateReferenceScriptParams = CreateReferenceScriptParams
  { createReferenceScriptScript :: Ledger.Script
  , createReferenceScriptAddress :: Address
  , createReferenceScriptUTxO :: Value
  } deriving (Generic,ToJSON,FromJSON)

data CreateSwapParams = CreateSwapParams
  { createSwapBeaconsMinted :: [[(TokenName,Integer)]]
  , createSwapBeaconRedeemers :: [BeaconRedeemer]
  , createSwapAddress :: Address
  , createSwapUTxOs :: [(Maybe SwapDatum, Value)]
  , createSwapAsInline :: Bool
  , createSwapScripts :: [DappScripts]
  , createSwapWithRefScripts :: Bool
  , createSwapRefScripts :: [TxOutRef]
  , createSwapRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data SwapParams = SwapParams
  { swapAddresses :: [Address]
  , swapSpecificUTxOs :: [TxOutRef]
  , swapChange :: [[(Maybe SwapDatum,Value)]]
  , swapAsInline :: Bool
  , swapScripts :: DappScripts -- ^ Only need the spending script which is the same for all swaps.
  , swapWithRefScript :: Bool
  , swapRefScript :: TxOutRef
  , swapRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data CloseOrUpdateParams = CloseOrUpdateParams
  { closeOrUpdateBeaconsBurned :: [[(TokenName,Integer)]]
  , closeOrUpdateBeaconRedeemer :: BeaconRedeemer
  , closeOrUpdateAddress :: Address
  , closeOrUpdateSpecificUTxOs :: [TxOutRef]
  , closeOrUpdateNewSwaps :: [(Maybe SwapDatum, Value)]
  , closeOrUpdateAsInline :: Bool
  , closeOrUpdateScripts :: [DappScripts]
  , closeOrUpdateWithRefScripts :: Bool
  , closeOrUpdateSpendRefScript :: TxOutRef
  , closeOrUpdateMintRefScripts :: [TxOutRef]
  , closeOrUpdateRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-reference-script" CreateReferenceScriptParams
  .\/ Endpoint "create-swap" CreateSwapParams
  .\/ Endpoint "swap" SwapParams
  .\/ Endpoint "close-or-update" CloseOrUpdateParams

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createReferenceScript :: CreateReferenceScriptParams -> Contract () TraceSchema Text ()
createReferenceScript CreateReferenceScriptParams{..} = do
  let d = Just $ TxOutDatumInline $ toDatum ()

      val = Validator $ createReferenceScriptScript

      refScript = unValidatorHash $ validatorHash val
      
      lookups = plutusV2OtherScript val

      tx' =
        mustPayToAddressWith createReferenceScriptAddress d (Just refScript) createReferenceScriptUTxO

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Reference script created"

createSwap :: CreateSwapParams -> Contract () TraceSchema Text ()
createSwap CreateSwapParams{..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet createSwapRefAddress
  let beaconRedeemers = map toRedeemer createSwapBeaconRedeemers
      
      toDatum'
        | createSwapAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      beaconPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy (beaconPolicy b))
                              (plutusV2MintingPolicy alwaysSucceedPolicy)
                              createSwapScripts

      lookups = beaconPolicies
             <> Constraints.unspentOutputs refUTxOs

      tx' =
        (if createSwapWithRefScripts then
          -- | Must reference scripts
          (mconcat $ map mustReferenceOutput createSwapRefScripts)
          -- | Mint beacons
          <> (mconcat $ 
                    zipWith4 
                      (\ref hs red toks ->
                        foldl' 
                          (\acc (t,i) -> acc <>
                            mustMintCurrencyWithRedeemerAndReference 
                              (Just ref) 
                              hs 
                              red 
                              t 
                              i
                          )
                          mempty
                          toks
                      )
                      createSwapRefScripts
                      (map beaconPolicyHash createSwapScripts)
                      beaconRedeemers
                      createSwapBeaconsMinted
                )
        else
          (mconcat $ zipWith3
              (\hs red toks -> 
                foldl' 
                  (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer hs red t i)
                  mempty
                  toks
              )
              (map beaconPolicyHash createSwapScripts)
              beaconRedeemers
              createSwapBeaconsMinted
          )
        )
        -- | Create Swap UTxOs
        <> (foldl'
              (\acc (d,v) -> acc <>
                mustPayToAddressWith createSwapAddress (fmap toDatum' d) Nothing v
              )
              mempty
              createSwapUTxOs
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swap(s) created"

swap :: SwapParams -> Contract () TraceSchema Text ()
swap SwapParams{swapScripts=DappScripts{..},..} = do
  swapUTxOs <- Map.unions
           <$> mapM (utxosAt . unsafeFromRight . toCardanoAddressInEra Mainnet) swapAddresses
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet swapRefAddress

  let swapRedeemer = toRedeemer Swap
      
      toDatum'
        | swapAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = Constraints.unspentOutputs swapUTxOs 
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs

      tx' = 
        -- | Must reference output
        mustReferenceOutput swapRefScript

        -- | Must spend all utxos to be swapped
        <> (if swapWithRefScript then
              foldl' (\a i -> a <> mustSpendScriptOutputWithReference i swapRedeemer swapRefScript) 
                     mempty
                     swapSpecificUTxOs
              
           else
              foldl' (\a i -> a <> mustSpendScriptOutput i swapRedeemer) 
                     mempty
                     swapSpecificUTxOs
           )

        -- | Return change to swap address
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) Nothing v)
              mempty
              b)
              swapAddresses
              swapChange
           )
           
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Assets swapped"

closeOrUpdate :: CloseOrUpdateParams -> Contract () TraceSchema Text ()
closeOrUpdate CloseOrUpdateParams{..} = do
  swapUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeOrUpdateAddress
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeOrUpdateRefAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let swapRedeemer = toRedeemer CloseOrUpdate
      beaconRedeemer = toRedeemer closeOrUpdateBeaconRedeemer

      toDatum'
        | closeOrUpdateAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      beaconPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy (beaconPolicy b))
                              (plutusV2MintingPolicy alwaysSucceedPolicy)
                              closeOrUpdateScripts

      lookups = beaconPolicies
             <> plutusV2OtherScript (spendingValidator $ closeOrUpdateScripts!!0)
             <> Constraints.unspentOutputs swapUTxOs
             <> Constraints.unspentOutputs refUTxOs

      tx' =
        -- | Must be signed by owner
        mustBeSignedBy userPubKeyHash

        -- | Create Swap UTxOs
        <> (foldl'
              (\acc (d,v) -> acc <>
                mustPayToAddressWith closeOrUpdateAddress (fmap toDatum' d) Nothing v
              )
              mempty
              closeOrUpdateNewSwaps
           )

        <> if closeOrUpdateWithRefScripts then
              -- | Must reference mint scripts
              (mconcat $ map mustReferenceOutput closeOrUpdateMintRefScripts)

              -- | Must reference spending script.
              <> mustReferenceOutput closeOrUpdateSpendRefScript

              -- | Burn beacons
              <> (mconcat $ 
                    zipWith3 
                      (\ref hs toks ->
                        foldl' 
                          (\acc (t,i) -> acc <>
                            mustMintCurrencyWithRedeemerAndReference 
                              (Just ref) 
                              hs 
                              beaconRedeemer 
                              t 
                              i
                          )
                          mempty
                          toks
                      )
                      closeOrUpdateMintRefScripts
                      (map beaconPolicyHash closeOrUpdateScripts)
                      closeOrUpdateBeaconsBurned
                )

              -- | Spend UTxOs to close
              <> (foldl' 
                    (\acc i -> acc <>
                      mustSpendScriptOutputWithReference
                        i
                        swapRedeemer
                        closeOrUpdateSpendRefScript
                    )
                    mempty
                    closeOrUpdateSpecificUTxOs
                )
          else
            (mconcat $ zipWith 
              (\hs toks -> 
                foldl' 
                  (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer hs beaconRedeemer t i)
                  mempty
                  toks
              )
              (map beaconPolicyHash closeOrUpdateScripts)
              closeOrUpdateBeaconsBurned
            )

            -- | Spend UTxOs to close
            <> (foldl' 
                  (\acc i -> acc <> mustSpendScriptOutput i swapRedeemer)
                  mempty
                  closeOrUpdateSpecificUTxOs
               )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Swaps closed/updated"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createReferenceScript' = endpoint @"create-reference-script" createReferenceScript
    createSwap' = endpoint @"create-swap" createSwap
    swap' = endpoint @"swap" swap
    closeOrUpdate' = endpoint @"close-or-update" closeOrUpdate
    choices = 
      [ createReferenceScript'
      , createSwap'
      , swap'
      , closeOrUpdate'
      ]