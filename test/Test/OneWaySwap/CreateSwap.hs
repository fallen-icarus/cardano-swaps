{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.CreateSwap
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4

    -- ** Scenarios that should fail
  , failureTest1
  , failureTest2
  , failureTest3
  , failureTest4
  , failureTest5
  , failureTest6
  , failureTest7
  , failureTest8
  , failureTest9
  , failureTest10
  , failureTest11
  , failureTest12
  , failureTest13
  , failureTest14
  , failureTest15
  , failureTest16
  , failureTest17
  , failureTest18
  , failureTest19
  , failureTest20
  , failureTest21
  , failureTest22
  , failureTest23
  , failureTest24
  , failureTest25
  , failureTest26
  , failureTest27
  , failureTest28
  , failureTest29
  , failureTest30
  , failureTest31
  , failureTest32
  , failureTest33
  , failureTest34
  , failureTest35
  , failureTest36
  , failureTest37
  , failureTest38
  , failureTest39
  , failureTest40
  , failureTest41
  , failureTest42
    
    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
    
    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Value.CardanoAPI as LV
import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.String (fromString)

import CardanoSwaps.OneWaySwap
import CardanoSwaps.Utils 

import Test.Prelude
import Test.OneWaySwap.UnsafeDatum

-------------------------------------------------
-- Initialize reference script.
-------------------------------------------------
initializeBeaconPolicy :: MonadEmulator m => m TxOutRef
initializeBeaconPolicy = do
  let w1 = Mock.knownMockWallet 1
  
  void $ transact (Mock.mockWalletAddress w1) [refScriptAddress] [Mock.paymentPrivateKey w1] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = refScriptAddress
              , outputValue = LV.lovelaceToValue 20000000
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript $ Just beaconScript
              }
          ]
      }

  txOutRefWithReferenceScript (scriptHash beaconScript)

-------------------------------------------------
-- Mint Test Tokens
-------------------------------------------------
mintTestTokens :: MonadEmulator m => Mock.MockWallet -> LV.Lovelace -> [(TokenName,Integer)] -> m ()
mintTestTokens w lovelace ts = do
  let walletAddress = Mock.mockWalletAddress w
  void $ transact walletAddress [refScriptAddress] [Mock.paymentPrivateKey w] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = ts
              , mintRedeemer = toRedeemer ()
              , mintPolicy = toVersioned alwaysSucceedPolicy
              , mintReference = Nothing
              }
          ]
      , outputs =
          [ Output
              { outputAddress = walletAddress
              , outputValue = utxoValue lovelace $
                  foldMap (uncurry $ PV2.singleton testTokenSymbol) ts
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      }

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Create a single valid Swap UTxO. The pair is (native token,ADA).
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Create multiple valid Swap UTxOs. All swaps are for the same trading pair.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,3),(offerBeacon,3),(askBeacon,3)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs = replicate 3 $
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , uncurry PV2.singleton (unOfferAsset offer) 10
                ]
            , outputDatum = OutputDatum $ toDatum swapDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
          
      , referenceInputs = [mintRef]
      }

-- | Create multiple valid Swap UTxOs. All swaps are for unique trading pairs.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap3 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Create multiple valid Swap UTxOs. All swaps are for unique trading pairs. Another policy
-- mints a token in the same transaction. This test checks that the beacon policy can properly
-- ignore other tokens being minted.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap3 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          , TokenMint
              { mintTokens = [("otherToken",1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersioned alwaysSucceedPolicy
              , mintReference = Nothing
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The trading pair corresponds to a different pair beacon than the one actually minted. The 
-- datum has the proper pair beacon name.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      wrongPairBeacon = genPairBeaconName (OfferAsset (testTokenSymbol,"TestToken2")) ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(wrongPairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol wrongPairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The trading pair corresponds to a different pair beacon than the one actually minted. The 
-- datum also has the wrong pair beacon name.
failureTest2 :: MonadEmulator m => m ()
failureTest2 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      wrongPairBeacon = genPairBeaconName (OfferAsset (testTokenSymbol,"TestToken2")) ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(wrongPairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol wrongPairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{pairBeacon = wrongPairBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The trading pair corresponds to a different pair beacon than the one in the datum. The proper
-- pair beacon was minted.
failureTest3 :: MonadEmulator m => m ()
failureTest3 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      correctPairBeacon = genPairBeaconName offer ask
      wrongPairBeacon = genPairBeaconName (OfferAsset (testTokenSymbol,"TestToken2")) ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(correctPairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol correctPairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{pairBeacon = wrongPairBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon and withdraw it.
failureTest4 :: MonadEmulator m => m ()
failureTest4 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,2),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon and store it in the swap UTxO.
failureTest5 :: MonadEmulator m => m ()
failureTest5 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,2),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 2
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The offer beacon minted corresponds to a different offer asset than the one in the trading
-- pair. The datum has the correct offer asset.
failureTest6 :: MonadEmulator m => m ()
failureTest6 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      wrongOfferBeacon = genOfferBeaconName $ OfferAsset (testTokenSymbol,"TestToken2")
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(wrongOfferBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol wrongOfferBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The offer beacon minted corresponds to a different offer asset than the one in the trading
-- pair. The datum also has the wrong offer asset.
failureTest7 :: MonadEmulator m => m ()
failureTest7 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      wrongOfferBeacon = genOfferBeaconName $ OfferAsset (testTokenSymbol,"TestToken2")
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(wrongOfferBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol wrongOfferBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{offerBeacon = wrongOfferBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The trading pair corresponds to a different offer beacon than the one in the datum. The proper
-- offer beacon was minted.
failureTest8 :: MonadEmulator m => m ()
failureTest8 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      wrongOfferBeacon = genOfferBeaconName (OfferAsset (testTokenSymbol,"TestToken2"))
      correctOfferBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(correctOfferBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol correctOfferBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{offerBeacon = wrongOfferBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional offer beacon and withdraw it.
failureTest9 :: MonadEmulator m => m ()
failureTest9 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,2),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional offer beacon and store it in the swap UTxO.
failureTest10 :: MonadEmulator m => m ()
failureTest10 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,2),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 2
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The ask beacon minted corresponds to a different ask asset than the one in the trading
-- pair. The datum has the correct ask asset.
failureTest11 :: MonadEmulator m => m ()
failureTest11 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      wrongAskBeacon = genAskBeaconName $ AskAsset (testTokenSymbol,"TestToken2")
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(wrongAskBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol wrongAskBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The ask beacon minted corresponds to a different ask asset than the one in the trading
-- pair. The datum also has the wrong ask asset.
failureTest12 :: MonadEmulator m => m ()
failureTest12 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      wrongAskBeacon = genAskBeaconName $ AskAsset (testTokenSymbol,"TestToken2")
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(wrongAskBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol wrongAskBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{askBeacon = wrongAskBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The trading pair corresponds to a different ask asset than the one in the datum. The 
-- proper ask beacon was still minted. 
failureTest13 :: MonadEmulator m => m ()
failureTest13 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      correctAskBeacon = genAskBeaconName ask
      wrongAskBeacon = genAskBeaconName $ AskAsset (testTokenSymbol,"TestToken2")
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(correctAskBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol correctAskBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{askBeacon = wrongAskBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional ask beacon and withdraw it.
failureTest14 :: MonadEmulator m => m ()
failureTest14 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,2)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional ask beacon and store it in the swap UTxO.
failureTest15 :: MonadEmulator m => m ()
failureTest15 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,2)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 2
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The beacon script mints an extra, unrelated token and it is withdrawn.
failureTest16 :: MonadEmulator m => m ()
failureTest16 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1),("other",1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The beacon script mints an extra, unrelated token and it is stored in the swap UTxO.
failureTest17 :: MonadEmulator m => m ()
failureTest17 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1),("other",1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , PV2.singleton beaconCurrencySymbol "other" 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, offer beacon, and ask beacon and store them together,
-- outside the swap address, but with the proper datum.
failureTest18 :: MonadEmulator m => m ()
failureTest18 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,2),(offerBeacon,2),(askBeacon,2)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = sellerPersonalAddr
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, offer beacon, and ask beacon and store them together,
-- outside the swap address, and without the proper datum.
failureTest19 :: MonadEmulator m => m ()
failureTest19 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,2),(offerBeacon,2),(askBeacon,2)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = sellerPersonalAddr
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, offer beacon, and ask beacon and store them together, in a
-- separate UTxO at the swap address, but without a datum.
failureTest20 :: MonadEmulator m => m ()
failureTest20 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,2),(offerBeacon,2),(askBeacon,2)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, offer beacon, and ask beacon and store them together, with
-- the proper datum but at a swap address without a staking credential.
failureTest21 :: MonadEmulator m => m ()
failureTest21 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

      -- Other Info
      swapAddressWithoutStaking = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,2),(offerBeacon,2),(askBeacon,2)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddressWithoutStaking
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Create a single valid Swap UTxO but store it at a swap address without staking.
failureTest22 :: MonadEmulator m => m ()
failureTest22 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddressWithoutStaking = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    Nothing

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddressWithoutStaking
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Create a single valid Swap UTxO but store it at a non-swap address.
failureTest23 :: MonadEmulator m => m ()
failureTest23 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      nonSwapAddress = refScriptAddress

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = nonSwapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong beacon id.
failureTest24 :: MonadEmulator m => m ()
failureTest24 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{beaconId = adaSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong offer id.
failureTest25 :: MonadEmulator m => m ()
failureTest25 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{offerId = adaSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong offer name.
failureTest26 :: MonadEmulator m => m ()
failureTest26 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{offerName = adaToken}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong ask id.
failureTest27 :: MonadEmulator m => m ()
failureTest27 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{askId = testTokenSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong ask name.
failureTest28 :: MonadEmulator m => m ()
failureTest28 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{askName = "Other"}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The offer asset and the ask asset are the same asset.
failureTest29 :: MonadEmulator m => m ()
failureTest29 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (testTokenSymbol,"TestToken1")
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a zero swap price.
failureTest30 :: MonadEmulator m => m ()
failureTest30 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 0 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a negative swap price.
failureTest31 :: MonadEmulator m => m ()
failureTest31 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio (-1) 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a zero denominator for swap price.
failureTest32 :: MonadEmulator m => m ()
failureTest32 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeOfferId = fst $ unOfferAsset offer
        , unsafeOfferName = snd $ unOfferAsset offer
        , unsafeOfferBeacon = offerBeacon
        , unsafeAskId = fst $ unAskAsset ask
        , unsafeAskName = snd $ unAskAsset ask
        , unsafeAskBeacon = askBeacon
        , unsafeSwapPrice = (1,0)
        , unsafePrevInput = Nothing
        }

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a negative denominator for swap price.
failureTest33 :: MonadEmulator m => m ()
failureTest33 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeOfferId = fst $ unOfferAsset offer
        , unsafeOfferName = snd $ unOfferAsset offer
        , unsafeOfferBeacon = offerBeacon
        , unsafeAskId = fst $ unAskAsset ask
        , unsafeAskName = snd $ unAskAsset ask
        , unsafeAskBeacon = askBeacon
        , unsafeSwapPrice = (1,-1)
        , unsafePrevInput = Nothing
        }

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap UTxO has an extraneous asset.
failureTest34 :: MonadEmulator m => m ()
failureTest34 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000),("TestToken2",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  , PV2.singleton testTokenSymbol "TestToken2" 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum is a datum hash.
failureTest35 :: MonadEmulator m => m ()
failureTest35 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = OutputDatumHash $ datumHash swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the pair beacons are mixed
-- up in the outputs.
failureTest36 :: MonadEmulator m => m ()
failureTest36 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the offer beacons are mixed
-- up in the outputs.
failureTest37 :: MonadEmulator m => m ()
failureTest37 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the ask beacons are mixed
-- up in the outputs.
failureTest38 :: MonadEmulator m => m ()
failureTest38 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (testTokenSymbol,"TestToken3")
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the swap datums are mixed
-- up.
failureTest39 :: MonadEmulator m => m ()
failureTest39 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap outputs, the first output is invalid. This test and the next one
-- are meant to check that the order of transaction outputs does not impact transaction validity.
failureTest40 :: MonadEmulator m => m ()
failureTest40 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap3 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{beaconId = adaSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap outputs, the second output is invalid. This test and the previous 
-- one are meant to check that the order of transaction outputs does not impact transaction 
-- validity.
failureTest41 :: MonadEmulator m => m ()
failureTest41 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap3 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{beaconId=adaSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The new swap UTxO does not have a datum.
failureTest42 :: MonadEmulator m => m ()
failureTest42 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),(askBeacon,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , uncurry PV2.singleton (unOfferAsset offer) 10
                  ]
              , outputDatum = NoOutputDatum 
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple swap UTxOs for the same trading pair. The trading pair is (native asset,ADA).
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 numberCreated = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon,fromIntegral numberCreated)
                  , (offerBeacon,fromIntegral numberCreated)
                  , (askBeacon,fromIntegral numberCreated)
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs = replicate numberCreated $
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , uncurry PV2.singleton (unOfferAsset offer) 10
                ]
            , outputDatum = OutputDatum $ toDatum swapDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
          
      , referenceInputs = [mintRef]
      }

-- | Create multiple swap UTxOs for the different trading pairs. 
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 numberCreated = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..80]

      -- Swap Info
      offers = map (\name -> OfferAsset (testTokenSymbol,name)) $ drop 40 assetNames
      asks = map (\name -> AskAsset (testTokenSymbol,name)) $ take 40 assetNames
      pairs = zip offers asks
      datums = take numberCreated $
        flip map pairs $ \(offer,ask) -> 
          genSwapDatum offer ask (unsafeRatio 1 1) Nothing

      sampleOutputs = flip map datums $ \datum@SwapDatum{..} ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , PV2.singleton offerId offerName 10
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 100_000_000 $ zip assetNames (repeat 1000)

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap datums $ \SwapDatum{..} ->
                  [ (pairBeacon,1)
                  , (offerBeacon,1)
                  , (askBeacon,1)
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs = sampleOutputs
      , referenceInputs = [mintRef]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all swap creation scenarios.
tests :: TestTree
tests =
  testGroup "Create Swap(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3
    , mustSucceed "regressionTest4" regressionTest4

      -- Failure Tests
    , scriptMustFailWithError "failureTest1" 
        "UTxO has wrong beacons" 
        failureTest1
    , scriptMustFailWithError "failureTest2" 
        "Wrong pair_beacon" 
        failureTest2
    , scriptMustFailWithError "failureTest3" 
        "UTxO has wrong beacons" 
        failureTest3
    , scriptMustFailWithError "failureTest4" 
        "One-way swaps must have exactly three kinds of beacons" 
        failureTest4
    , scriptMustFailWithError "failureTest5" 
        "UTxO has wrong beacons" 
        failureTest5
    , scriptMustFailWithError "failureTest6" 
        "UTxO has wrong beacons" 
        failureTest6
    , scriptMustFailWithError "failureTest7" 
        "Wrong offer_beacon" 
        failureTest7
    , scriptMustFailWithError "failureTest8" 
        "UTxO has wrong beacons" 
        failureTest8
    , scriptMustFailWithError "failureTest9" 
        "One-way swaps must have exactly three kinds of beacons" 
        failureTest9
    , scriptMustFailWithError "failureTest10" 
        "UTxO has wrong beacons" 
        failureTest10
    , scriptMustFailWithError "failureTest11" 
        "UTxO has wrong beacons" 
        failureTest11
    , scriptMustFailWithError "failureTest12" 
        "Wrong ask_beacon" 
        failureTest12
    , scriptMustFailWithError "failureTest13" 
        "UTxO has wrong beacons" 
        failureTest13
    , scriptMustFailWithError "failureTest14" 
        "One-way swaps must have exactly three kinds of beacons" 
        failureTest14
    , scriptMustFailWithError "failureTest15" 
        "UTxO has wrong beacons" 
        failureTest15
    , scriptMustFailWithError "failureTest16" 
        "One-way swaps must have exactly three kinds of beacons" 
        failureTest16
    , scriptMustFailWithError "failureTest17" 
        "One-way swaps must have exactly three kinds of beacons" 
        failureTest17
    , scriptMustFail "failureTest18" failureTest18
    , scriptMustFail "failureTest19" failureTest19
    , scriptMustFailWithError "failureTest20" 
        "All swap datums must be inline datums" 
        failureTest20
    , scriptMustFail "failureTest21" failureTest21
    , scriptMustFail "failureTest22" failureTest22
    , scriptMustFail "failureTest23" failureTest23
    , scriptMustFailWithError "failureTest24" 
        "Wrong beacon_id" 
        failureTest24
    , scriptMustFailWithError "failureTest25" 
        "No extraneous assets allowed in the UTxO" 
        failureTest25
    , scriptMustFailWithError "failureTest26" 
        "No extraneous assets allowed in the UTxO" 
        failureTest26
    , scriptMustFailWithError "failureTest27" 
        "Wrong pair_beacon" 
        failureTest27
    , scriptMustFailWithError "failureTest28" 
        "Wrong pair_beacon" 
        failureTest28
    , scriptMustFailWithError "failureTest29" 
        "Offer asset cannot be same as ask asset" 
        failureTest29
    , scriptMustFailWithError "failureTest30" 
        "swap_price numerator not > 0" 
        failureTest30
    , scriptMustFailWithError "failureTest31" 
        "swap_price numerator not > 0" 
        failureTest31
    , scriptMustFailWithError "failureTest32" 
        "swap_price denominator not > 0" 
        failureTest32
    , scriptMustFailWithError "failureTest33" 
        "swap_price denominator not > 0" 
        failureTest33
    , scriptMustFailWithError "failureTest34" 
        "No extraneous assets allowed in the UTxO" 
        failureTest34
    , scriptMustFailWithError "failureTest35" 
        "All swap datums must be inline datums" 
        failureTest35
    , scriptMustFailWithError "failureTest36" 
        "UTxO has wrong beacons" 
        failureTest36
    , scriptMustFailWithError "failureTest37" 
        "UTxO has wrong beacons" 
        failureTest37
    , scriptMustFailWithError "failureTest38" 
        "UTxO has wrong beacons" 
        failureTest38
    , scriptMustFailWithError "failureTest39" 
        "UTxO has wrong beacons" 
        failureTest39
    , scriptMustFailWithError "failureTest40" 
        "Wrong beacon_id" 
        failureTest40
    , scriptMustFailWithError "failureTest41" 
        "Wrong beacon_id" 
        failureTest41
    , scriptMustFailWithError "failureTest42" 
        "All swap datums must be inline datums" 
        failureTest42

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 35
    , mustSucceed "benchTest2" $ benchTest2 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 36
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 25
    ]
