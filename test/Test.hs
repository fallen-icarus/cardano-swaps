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

-- import Test.BenchSwaps

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

  --     cfgs = [ SwapConfig ("","") testToken1
  --            , SwapConfig testToken1 testToken2
  --            , SwapConfig testToken2 testToken3
  --            , SwapConfig testToken3 testToken4
  --            , SwapConfig testToken4 testToken5
  --            , SwapConfig testToken5 testToken6
  --            , SwapConfig testToken6 testToken7
  --            , SwapConfig testToken7 testToken8
  --            , SwapConfig testToken8 testToken9
  --            , SwapConfig testToken9 testToken10
  --            , SwapConfig testToken10 testToken11
  --            , SwapConfig testToken11 testToken12
  --            , SwapConfig testToken12 testToken13
  --            , SwapConfig testToken13 testToken14
  --            , SwapConfig testToken14 testToken15
  --            , SwapConfig testToken15 testToken16
  --            , SwapConfig testToken16 testToken17
  --            ]
  --     benchScripts = map (\z -> genScripts z blueprints) cfgs

  -- benchTrace benchScripts

  

  -- print $ scriptSize $ unValidatorScript $ spendingValidator testScripts
  -- Swap.testTrace testScripts

  defaultMain $ testGroup "Cardano-Swaps"
    [ uniqueBeaconsTest blueprints
    , OpenSwapAddress.tests testScripts
    , CloseSwapAddress.tests testScripts
    , Update.tests testScripts
    , Swap.tests testScripts
    ]