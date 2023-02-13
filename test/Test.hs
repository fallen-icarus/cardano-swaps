module Main where

import Test.Tasty

import Test.CreateAddress as CreateAddress
import Test.BurningBeacons as BurningBeacons

main :: IO ()
main = defaultMain $ testGroup "Cardano-Swaps"
  [
    CreateAddress.tests,
    BurningBeacons.tests
  ]