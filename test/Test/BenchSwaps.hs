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

module Test.BenchSwaps
(
  benchTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import Control.Monad (void)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address

import Test.Common
import CardanoSwaps

-------------------------------------------------
-- Scenarios
-------------------------------------------------
chainSwapsWithoutReferenceScripts :: [DappScripts] -> EmulatorTrace ()
chainSwapsWithoutReferenceScripts ts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let syms = map beaconCurrencySymbol ts
      priceDatum1 = SwapPrice $ unsafeRatio 1 1_000_000
      beaconDatum1 = BeaconSymbol $ syms!!0
      priceDatum2 = SwapPrice $ unsafeRatio 1 1
      beaconDatum2 = BeaconSymbol $ syms!!1
      priceDatum3 = SwapPrice $ unsafeRatio 1 1
      beaconDatum3 = BeaconSymbol $ syms!!2
      priceDatum4 = SwapPrice $ unsafeRatio 1 1
      beaconDatum4 = BeaconSymbol $ syms!!3

      addrs = map (\z -> Address (ScriptCredential $ spendingValidatorHash z)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )
                  ) ts

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!0
      , openSwapAddressInfo =
          [ ( Just beaconDatum1
            , lovelaceValueOf 20_000_000 <> singleton (syms!!0) "" 1
            )
          , ( Just priceDatum1
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!0
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 2

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!1
      , openSwapAddressInfo =
          [ ( Just beaconDatum2
            , lovelaceValueOf 20_000_000 <> singleton (syms!!1) "" 1
            )
          , ( Just priceDatum2
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!1
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 4

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!2
      , openSwapAddressInfo =
          [ ( Just beaconDatum3
            , lovelaceValueOf 20_000_000 <> singleton (syms!!2) "" 1
            )
          , ( Just priceDatum3
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken2) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!2
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 6

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!3
      , openSwapAddressInfo =
          [ ( Just beaconDatum4
            , lovelaceValueOf 20_000_000 <> singleton (syms!!3) "" 1
            )
          , ( Just priceDatum4
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken3) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!3
      , openSwapAddressWithRefScript = False
      , openSwapRefUTxO = []
      }

  void $ waitUntilSlot 8

  inputA1 <- txOutRefWithValue $ lovelaceValueOf 10_000_000
  inputB1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken1) 50
  inputC1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken2) 50
  inputD1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken3) 50

  callEndpoint @"chain-swaps" h2 $
    ChainSwapParams
      { chainSwapAddresses = addrs
      , chainSwapSpecificUTxOs = 
          [ [inputA1]
          , [inputB1]
          , [inputC1]
          , [inputD1]
          ]
      , chainSwapChange =
          [ [ ( Just priceDatum1
              , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
              )
            ]
          , [ ( Just priceDatum2
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken2) 50
              )
            ]
          , [ ( Just priceDatum3
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken3) 50
              )
            ]
          , [ ( Just priceDatum4
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken4) 50
              )
            ]
          ]
      , chainScripts = ts
      , chainWithRefScripts = False
      , chainRefScripts = []
      }

chainSwapsWithReferenceScripts :: [DappScripts] -> EmulatorTrace ()
chainSwapsWithReferenceScripts ts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let syms = map beaconCurrencySymbol ts
      priceDatum1 = SwapPrice $ unsafeRatio 1 1_000_000
      beaconDatum1 = BeaconSymbol $ syms!!0
      priceDatum2 = SwapPrice $ unsafeRatio 1 1
      beaconDatum2 = BeaconSymbol $ syms!!1
      priceDatum3 = SwapPrice $ unsafeRatio 1 1
      beaconDatum3 = BeaconSymbol $ syms!!2
      priceDatum4 = SwapPrice $ unsafeRatio 1 1
      beaconDatum4 = BeaconSymbol $ syms!!3
      priceDatum5 = SwapPrice $ unsafeRatio 1 1
      beaconDatum5 = BeaconSymbol $ syms!!4
      priceDatum6 = SwapPrice $ unsafeRatio 1 1
      beaconDatum6 = BeaconSymbol $ syms!!5
      priceDatum7 = SwapPrice $ unsafeRatio 1 1
      beaconDatum7 = BeaconSymbol $ syms!!6
      priceDatum8 = SwapPrice $ unsafeRatio 1 1
      beaconDatum8 = BeaconSymbol $ syms!!7
      priceDatum9 = SwapPrice $ unsafeRatio 1 1
      beaconDatum9 = BeaconSymbol $ syms!!8
      priceDatum10 = SwapPrice $ unsafeRatio 1 1
      beaconDatum10 = BeaconSymbol $ syms!!9
      priceDatum11 = SwapPrice $ unsafeRatio 1 1
      beaconDatum11 = BeaconSymbol $ syms!!10
      priceDatum12 = SwapPrice $ unsafeRatio 1 1
      beaconDatum12 = BeaconSymbol $ syms!!11
      priceDatum13 = SwapPrice $ unsafeRatio 1 1
      beaconDatum13 = BeaconSymbol $ syms!!12
      priceDatum14 = SwapPrice $ unsafeRatio 1 1
      beaconDatum14 = BeaconSymbol $ syms!!13
      priceDatum15 = SwapPrice $ unsafeRatio 1 1
      beaconDatum15 = BeaconSymbol $ syms!!14

      addrs = map (\z -> Address (ScriptCredential $ spendingValidatorHash z)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1
                     )
                  ) ts

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!0
      , openSwapAddressInfo =
          [ ( Just priceDatum1
            , lovelaceValueOf 10_000_000
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!0
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum1
            , lovelaceValueOf 23_000_000 <> singleton (syms!!0) "" 1
            )
          ]
      }

  void $ waitUntilSlot 2

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!1
      , openSwapAddressInfo =
          [ ( Just priceDatum2
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken1) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!1
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum2
            , lovelaceValueOf 23_000_000 <> singleton (syms!!1) "" 1
            )
          ]
      }

  void $ waitUntilSlot 4

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!2
      , openSwapAddressInfo =
          [ ( Just priceDatum3
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken2) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!2
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum3
            , lovelaceValueOf 23_000_000 <> singleton (syms!!2) "" 1
            )
          ]
      }

  void $ waitUntilSlot 6

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!3
      , openSwapAddressInfo =
          [ ( Just priceDatum4
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken3) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!3
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum4
            , lovelaceValueOf 23_000_000 <> singleton (syms!!3) "" 1
            )
          ]
      }

  void $ waitUntilSlot 8

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!4
      , openSwapAddressInfo =
          [ ( Just priceDatum5
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken4) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!4
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum5
            , lovelaceValueOf 23_000_000 <> singleton (syms!!4) "" 1
            )
          ]
      }

  void $ waitUntilSlot 10

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!5
      , openSwapAddressInfo =
          [ ( Just priceDatum6
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken5) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!5
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum6
            , lovelaceValueOf 23_000_000 <> singleton (syms!!5) "" 1
            )
          ]
      }

  void $ waitUntilSlot 12

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!6
      , openSwapAddressInfo =
          [ ( Just priceDatum7
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken6) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!6
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum7
            , lovelaceValueOf 23_000_000 <> singleton (syms!!6) "" 1
            )
          ]
      }

  void $ waitUntilSlot 14

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!7
      , openSwapAddressInfo =
          [ ( Just priceDatum8
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken7) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!7
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum8
            , lovelaceValueOf 23_000_000 <> singleton (syms!!7) "" 1
            )
          ]
      }

  void $ waitUntilSlot 16

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!8
      , openSwapAddressInfo =
          [ ( Just priceDatum9
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken8) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!8
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum9
            , lovelaceValueOf 23_000_000 <> singleton (syms!!8) "" 1
            )
          ]
      }

  void $ waitUntilSlot 18

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!9
      , openSwapAddressInfo =
          [ ( Just priceDatum10
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken9) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!9
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum10
            , lovelaceValueOf 23_000_000 <> singleton (syms!!9) "" 1
            )
          ]
      }

  void $ waitUntilSlot 20

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!10
      , openSwapAddressInfo =
          [ ( Just priceDatum11
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken10) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!10
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum11
            , lovelaceValueOf 23_000_000 <> singleton (syms!!10) "" 1
            )
          ]
      }

  void $ waitUntilSlot 22

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!11
      , openSwapAddressInfo =
          [ ( Just priceDatum12
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken11) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!11
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum12
            , lovelaceValueOf 23_000_000 <> singleton (syms!!11) "" 1
            )
          ]
      }

  void $ waitUntilSlot 24

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!12
      , openSwapAddressInfo =
          [ ( Just priceDatum13
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken12) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!12
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum13
            , lovelaceValueOf 23_000_000 <> singleton (syms!!12) "" 1
            )
          ]
      }

  void $ waitUntilSlot 26

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!13
      , openSwapAddressInfo =
          [ ( Just priceDatum14
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken13) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!13
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum14
            , lovelaceValueOf 23_000_000 <> singleton (syms!!13) "" 1
            )
          ]
      }

  void $ waitUntilSlot 28

  callEndpoint @"open-swap-address" h1 $
    OpenSwapAddressParams
      { openSwapAddressBeaconsMinted = [("",1)]
      , openSwapAddressBeaconRedeemer = MintBeacon
      , openSwapAddressAddress = addrs!!14
      , openSwapAddressInfo =
          [ ( Just priceDatum15
            , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken14) 50
            )
          ]
      , openSwapAddressAsInline = True
      , openSwapAddressScripts = ts!!14
      , openSwapAddressWithRefScript = True
      , openSwapRefUTxO = 
          [ ( Just beaconDatum15
            , lovelaceValueOf 23_000_000 <> singleton (syms!!14) "" 1
            )
          ]
      }

  void $ waitUntilSlot 30

  inputA1 <- txOutRefWithValue $ lovelaceValueOf 10_000_000
  inputB1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken1) 50
  inputC1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken2) 50
  inputD1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken3) 50
  inputE1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken4) 50
  inputF1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken5) 50
  inputG1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken6) 50
  inputH1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken7) 50
  inputI1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken8) 50
  inputJ1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken9) 50
  inputK1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken10) 50
  inputL1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken11) 50
  inputM1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken12) 50
  inputN1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken13) 50
  inputO1 <- txOutRefWithValue $ lovelaceValueOf 2_000_000 <> (uncurry singleton testToken14) 50
  refA <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!0) "" 1
  refB <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!1) "" 1
  refC <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!2) "" 1
  refD <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!3) "" 1
  refE <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!4) "" 1
  refF <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!5) "" 1
  refG <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!6) "" 1
  refH <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!7) "" 1
  refI <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!8) "" 1
  refJ <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!9) "" 1
  refK <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!10) "" 1
  refL <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!11) "" 1
  refM <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!12) "" 1
  refN <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!13) "" 1
  refO <- txOutRefWithValue $ lovelaceValueOf 23_000_000 <> singleton (syms!!14) "" 1

  callEndpoint @"chain-swaps" h2 $
    ChainSwapParams
      { chainSwapAddresses = addrs
      , chainSwapSpecificUTxOs = 
          [ [inputA1]
          , [inputB1]
          , [inputC1]
          , [inputD1]
          , [inputE1]
          , [inputF1]
          , [inputG1]
          , [inputH1]
          , [inputI1]
          , [inputJ1]
          , [inputK1]
          , [inputL1]
          , [inputM1]
          , [inputN1]
          ]
      , chainSwapChange =
          [ [ ( Just priceDatum1
              , lovelaceValueOf 5_000_000 <> (uncurry singleton testToken1) 50
              )
            ]
          , [ ( Just priceDatum2
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken2) 50
              )
            ]
          , [ ( Just priceDatum3
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken3) 50
              )
            ]
          , [ ( Just priceDatum4
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken4) 50
              )
            ]
          , [ ( Just priceDatum5
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken5) 50
              )
            ]
          , [ ( Just priceDatum6
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken6) 50
              )
            ]
          , [ ( Just priceDatum7
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken7) 50
              )
            ]
          , [ ( Just priceDatum8
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken8) 50
              )
            ]
          , [ ( Just priceDatum9
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken9) 50
              )
            ]
          , [ ( Just priceDatum10
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken10) 50
              )
            ]
          , [ ( Just priceDatum11
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken11) 50
              )
            ]
          , [ ( Just priceDatum12
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken12) 50
              )
            ]
          , [ ( Just priceDatum13
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken13) 50
              )
            ]
          , [ ( Just priceDatum14
              , lovelaceValueOf 2_000_000 <> (uncurry singleton testToken14) 50
              )
            ]
          ]
      , chainScripts = ts
      , chainWithRefScripts = True
      , chainRefScripts = 
          [refA,refB,refC,refD,refE,refF,refG,refH,refI,refJ,refK,refL,refM,refN]
      }

benchTrace :: [DappScripts] -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . chainSwapsWithReferenceScripts