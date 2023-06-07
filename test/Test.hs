{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Ledger (scriptSize,unValidatorScript)
import Test.Tasty
import Test.Tasty.HUnit

import CardanoSwaps
import Test.Common
import Test.OpenSwapAddress as OpenSwapAddress
import Test.CloseSwapAddress as CloseSwapAddress
import Test.Update as Update
import Test.Swap as Swap

uniqueBeaconsTest :: Blueprints -> TestTree
uniqueBeaconsTest bs = testGroup "Beacon Uniqueness"
  [ testCase "Unique beacons for every SwapConfig" $ assertBool "Beacons are not unique" $
      beaconCurrencySymbol (genScripts (SwapConfig ("","") testToken2) bs) /= 
        beaconCurrencySymbol (genScripts (SwapConfig ("","") testToken1) bs)
  ]

main :: IO ()
main = do
  blueprints <- readBlueprints "aiken/plutus.json"
  let cfg = SwapConfig ("","") testToken1
      testScripts = genScripts cfg blueprints

  -- print $ scriptSize $ unValidatorScript $ spendingValidator testScripts
  -- Swap.testTrace testScripts

  defaultMain $ testGroup "Cardano-Swaps"
    [ uniqueBeaconsTest blueprints
    , OpenSwapAddress.tests testScripts
    , CloseSwapAddress.tests testScripts
    , Update.tests testScripts
    , Swap.tests testScripts
    ]