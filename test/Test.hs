{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ledger (scriptSize,unValidatorScript)
import Test.Tasty
import Test.Tasty.HUnit

import CardanoSwaps
import Test.Common
import Test.CreateSwap as CreateSwap
import Test.Swap as Swap
import Test.CloseOrUpdate as CloseOrUpdate

uniqueBeaconsTest :: Blueprints -> TestTree
uniqueBeaconsTest bs = testGroup "Beacon Uniqueness"
  [ testCase "Unique beacons for every AssetConfig" $ assertBool "Beacons are not unique" $
      beaconCurrencySymbol (genScripts (AssetConfig "" "") bs) /= 
        beaconCurrencySymbol (genScripts (AssetConfig "" "abc") bs)
  ]

main :: IO ()
main = do
  blueprints <- readBlueprints "aiken/plutus.json"
  let cfgs = [ uncurry AssetConfig ("","")
             , uncurry AssetConfig testToken1
             , uncurry AssetConfig testToken2
             , uncurry AssetConfig testToken3
             , uncurry AssetConfig testToken4
             , uncurry AssetConfig testToken5
             , uncurry AssetConfig testToken6
             , uncurry AssetConfig testToken7
             , uncurry AssetConfig testToken8
             , uncurry AssetConfig testToken9
             , uncurry AssetConfig testToken10
             , uncurry AssetConfig testToken11
             , uncurry AssetConfig testToken12
             , uncurry AssetConfig testToken13
             , uncurry AssetConfig testToken14
             , uncurry AssetConfig testToken15
             , uncurry AssetConfig testToken16
             , uncurry AssetConfig testToken17
             , uncurry AssetConfig testToken18
             , uncurry AssetConfig testToken19
             , uncurry AssetConfig testToken20
             , uncurry AssetConfig testToken21
             , uncurry AssetConfig testToken22
             , uncurry AssetConfig testToken23
             , uncurry AssetConfig testToken24
             , uncurry AssetConfig testToken25
             , uncurry AssetConfig testToken26
             , uncurry AssetConfig testToken27
             , uncurry AssetConfig testToken28
             , uncurry AssetConfig testToken29
             , uncurry AssetConfig testToken30
             ]
      scripts = map (\z -> genScripts z blueprints) cfgs

  -- print $ scriptSize $ unValidatorScript $ spendingValidator testScripts
  -- CloseOrUpdate.testTrace scripts

  defaultMain $ testGroup "Cardano-Swaps"
    [ uniqueBeaconsTest blueprints
    , CreateSwap.tests scripts
    , Swap.tests scripts
    , CloseOrUpdate.tests scripts
    ]