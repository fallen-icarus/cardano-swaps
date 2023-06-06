{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Plutus.Script.Utils.V2.Scripts as UScripts
import Ledger (Validator(..),MintingPolicy(..),scriptSize,unValidatorScript)
import Test.Tasty
import Test.Tasty.HUnit

import CardanoSwaps
import Test.Common
import Test.OpenSwapAddress as OpenSwapAddress
import Test.CloseSwapAddress as CloseSwapAddress
import Test.Update as Update
import Test.Swap as Swap

genTestScripts :: SwapConfig -> Blueprints -> TestScripts
genTestScripts cfg bs = TestScripts
    { spendingValidator = spendVal
    , spendingValidatorHash = spendValHash
    , beaconPolicy = beacon
    , beaconPolicyHash = beaconHash
    , beaconCurrencySymbol = scriptCurrencySymbol beacon
    }
  where spendVal = Validator $ applySwapParams cfg $ bs Map.! "cardano_swaps.spend"
        spendValHash = UScripts.validatorHash spendVal
        beacon = MintingPolicy $ applyBeaconParams spendValHash $ bs Map.! "cardano_swaps.mint"
        beaconHash = mintingPolicyHash beacon

uniqueBeaconsTest :: Blueprints -> TestTree
uniqueBeaconsTest bs = testGroup "Beacon Uniqueness"
  [ testCase "Unique beacons for every SwapConfig" $ assertBool "Beacons are not unique" $
      beaconCurrencySymbol (genTestScripts (SwapConfig ("","") testToken2) bs) /= 
        beaconCurrencySymbol (genTestScripts (SwapConfig ("","") testToken1) bs)
  ]

main :: IO ()
main = do
  blueprints <- readBlueprints "aiken/plutus.json"
  let cfg = SwapConfig ("","") testToken1
      testScripts = genTestScripts cfg blueprints

  -- print $ scriptSize $ unValidatorScript $ spendingValidator testScripts
  -- Swap.testTrace testScripts

  defaultMain $ testGroup "Cardano-Swaps"
    [ uniqueBeaconsTest blueprints
    , OpenSwapAddress.tests testScripts
    , CloseSwapAddress.tests testScripts
    , Update.tests testScripts
    , Swap.tests testScripts
    ]