{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Config where

import qualified Data.Map as Map
import Wallet.Emulator.Wallet
import qualified Cardano.Api as C
import Ledger.Tx.CardanoAPI.Internal (toCardanoValue)
import Plutus.Trace.Emulator

import Test.Internal
import CardanoSwaps.Utils

-------------------------------------------------
-- Configs
-------------------------------------------------
-- | An always succeeding validator address without a staking credential.
refScriptAddress :: Address
refScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

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

testToken48 :: (CurrencySymbol,TokenName)
testToken48 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken48")

testToken49 :: (CurrencySymbol,TokenName)
testToken49 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken49")

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
         <> (uncurry singleton testToken41) 1000
         <> (uncurry singleton testToken42) 1000
         <> (uncurry singleton testToken43) 1000
         <> (uncurry singleton testToken44) 1000
         <> (uncurry singleton testToken45) 1000
         <> (uncurry singleton testToken46) 1000
         <> (uncurry singleton testToken47) 1000
         <> (uncurry singleton testToken48) 1000
         <> (uncurry singleton testToken49) 1000

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
    protoParams = def
      { 
        protocolParamMaxTxExUnits = 
          Just (C.ExecutionUnits { C.executionSteps = 10000000000
                                 , C.executionMemory = 13500000
                                 }
               )
      , protocolParamMaxTxSize = 11300
      }
