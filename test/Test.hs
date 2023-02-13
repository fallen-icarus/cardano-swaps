module Main where

import Test.Tasty

import Test.CreateAddress as CreateAddress
import Test.BurningBeacons as BurningBeacons
import Test.CloseAddress as CloseAddress
import Test.UpdateSwaps as UpdateSwaps
import Test.SwapAssets as SwapAssets

main :: IO ()
main = defaultMain $ testGroup "Cardano-Swaps"
  [
    CreateAddress.tests,
    BurningBeacons.tests,
    CloseAddress.tests,
    UpdateSwaps.tests,
    SwapAssets.tests
  ]