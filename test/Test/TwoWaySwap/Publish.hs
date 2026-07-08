{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TwoWaySwap.Publish
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1

    -- ** Scenarios that should fail
  , failureTest1

    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Value.CardanoAPI as LV
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock
import Test.Tasty (TestTree,testGroup)

import CardanoSwaps.TwoWaySwap
import CardanoSwaps.Utils

import Test.Prelude

-------------------------------------------------
-- Initialize reference scripts.
-------------------------------------------------
initializeBeaconScript :: MonadEmulator m => m TxOutRef
initializeBeaconScript = do
  let w1 = Mock.knownMockWallet 1

  void $ transact (Mock.mockWalletAddress w1) [refScriptAddress] [Mock.paymentPrivateKey w1] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = refScriptAddress
              , outputValue = LV.lovelaceToValue 21_000_000
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript $ Just beaconScript
              }
          ]
      }

  txOutRefWithReferenceScript (scriptHash beaconScript)

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Register the beacon script's staking credential.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let w1 = Mock.knownMockWallet 1

  mintRef <- initializeBeaconScript

  -- Try to register the beacon script.
  void $ transact (Mock.mockWalletAddress w1) [refScriptAddress] [Mock.paymentPrivateKey w1] $
    emptyTxParams
      { certificates =
          [ Certificate
              { certificateCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , certificateWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              , certificateAction = Register
              }
          ]
      , referenceInputs = [mintRef]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | Deregister the beacon script's staking credential. The publish handler only permits
-- registration.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
  let w1 = Mock.knownMockWallet 1

  mintRef <- initializeBeaconScript

  -- Register the beacon script.
  void $ transact (Mock.mockWalletAddress w1) [refScriptAddress] [Mock.paymentPrivateKey w1] $
    emptyTxParams
      { certificates =
          [ Certificate
              { certificateCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , certificateWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              , certificateAction = Register
              }
          ]
      , referenceInputs = [mintRef]
      }

  -- Try to deregister the beacon script.
  void $ transact (Mock.mockWalletAddress w1) [refScriptAddress] [Mock.paymentPrivateKey w1] $
    emptyTxParams
      { certificates =
          [ Certificate
              { certificateCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , certificateWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              , certificateAction = UnRegister
              }
          ]
      , referenceInputs = [mintRef]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all beacon script publish scenarios.
tests :: TestTree
tests =
  testGroup "Publish"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1

      -- Failure Tests
    , scriptMustFailWithError "failureTest1"
        "Publish can only be used to register the beacon script"
        failureTest1
    ]
