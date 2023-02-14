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

module Test.BurningBeacons
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.Default
import Plutus.V2.Ledger.Api

import Test.Common

-------------------------------------------------
-- Beacon Burning Scenarios
-------------------------------------------------
burnSingleBeacon :: EmulatorTrace ()
burnSingleBeacon = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"burn-beacons" h1 $
    BurnBeaconParams
      {
        beaconsBurned = [(adaToken,-1)]
      , useBurnRedeemer = True
      }

burnMulitpleBeacons :: EmulatorTrace ()
burnMulitpleBeacons = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"burn-beacons" h1 $
    BurnBeaconParams
      {
        beaconsBurned = [(adaToken,-5)]
      , useBurnRedeemer = True
      }

burnWithMintRedeemer :: EmulatorTrace ()
burnWithMintRedeemer = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"burn-beacons" h1 $
    BurnBeaconParams
      {
        beaconsBurned = [(adaToken,-1)]
      , useBurnRedeemer = False
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Burning Beacons"
    [ -- | Always allow burning.
      checkPredicateOptions opts "Allows burning a single beacon"
        assertNoFailedTransactions burnSingleBeacon
    , checkPredicateOptions opts "Allows burning multiple beacons"
        assertNoFailedTransactions burnMulitpleBeacons

      -- | Must use burn redeemer.
    , checkPredicateOptions opts "Fail if mint redeemer used to burn"
        (Test.not assertNoFailedTransactions) burnWithMintRedeemer
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig burnWithMintRedeemer