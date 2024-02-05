{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TwoWaySwap.CreateSwap
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5

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
  , failureTest43
  , failureTest44
  , failureTest45
  , failureTest46
  , failureTest47
  , failureTest48
    
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

import CardanoSwaps.TwoWaySwap
import CardanoSwaps.Utils 

import Test.Prelude
import Test.TwoWaySwap.UnsafeDatum

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
              , outputValue = LV.lovelaceToValue 22000000
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                , uncurry PV2.singleton offer 10
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
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer1 10
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
                  , uncurry PV2.singleton offer2 10
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
                  , uncurry PV2.singleton offer3 10
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
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer1 10
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
                  , uncurry PV2.singleton offer2 10
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
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Create a single valid Swap UTxO. The UTxO has both asset1 and asset2.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (testTokenSymbol,"TestToken2")
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 1 1) Nothing

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
                  , uncurry PV2.singleton offer 10
                  , uncurry PV2.singleton ask 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      wrongPairBeacon = genPairBeaconName (testTokenSymbol,"TestToken2") ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      wrongPairBeacon = genPairBeaconName (testTokenSymbol,"TestToken2") ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      correctPairBeacon = genPairBeaconName offer ask
      wrongPairBeacon = genPairBeaconName (testTokenSymbol,"TestToken2") ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The asset2 beacon minted corresponds to a different asset than the one in the trading
-- pair. The datum has the correct asset2 asset.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      wrongOfferBeacon = genAssetBeaconName (testTokenSymbol,"TestToken2")
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The asset2 beacon minted corresponds to a different asset than the one in the trading
-- pair. The datum also has the wrong asset2 asset.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      wrongOfferBeacon = genAssetBeaconName (testTokenSymbol,"TestToken2")
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset2Beacon = wrongOfferBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The trading pair corresponds to a different asset2 beacon than the one in the datum. The proper
-- asset2 beacon was minted.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      wrongOfferBeacon = genAssetBeaconName (testTokenSymbol,"TestToken2")
      correctOfferBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset2Beacon = wrongOfferBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional asset2 beacon and withdraw it.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional asset2 beacon and store it in the swap UTxO.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The asse1 beacon minted corresponds to a different asset than the one in the trading
-- pair. The datum has the correct asset1 asset.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      wrongAskBeacon = genAssetBeaconName (testTokenSymbol,"TestToken2")
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The asset1 beacon minted corresponds to a different asset than the one in the trading
-- pair. The datum also has the wrong asset1 asset.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      wrongAskBeacon = genAssetBeaconName (testTokenSymbol,"TestToken2")
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Beacon = wrongAskBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The trading pair corresponds to a different asset1 than the one in the datum. The 
-- proper asset1 beacon was still minted. 
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      correctAskBeacon = genAssetBeaconName ask
      wrongAskBeacon = genAssetBeaconName  (testTokenSymbol,"TestToken2")
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Beacon = wrongAskBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional asset1 beacon and withdraw it.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional asset1 beacon and store it in the swap UTxO.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, asset2 beacon, and asset1 beacon and store them together,
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, asset2 beacon, and asset1 beacon and store them together,
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum =
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, asset2 beacon, and asset1 beacon and store them together, in a
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Mint an additional pair beacon, asset2 beacon, and asset1 beacon and store them together, with
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{beaconId = adaSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong asset2 id.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset2Id = adaSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong asset2 name.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset2Name = adaToken}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong asset1 id.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Id = testTokenSymbol}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum has the wrong asset1 name.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Name = "Other"}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Asset1 and asset2 are the same asset. The beacons are properly minted.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (testTokenSymbol,"TestToken1")
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Asset1 and asset2 are the same asset. An extra beacon is minted and stored in the UTxO.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (testTokenSymbol,"TestToken1")
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

  -- Initialize scenario
  mintRef <- initializeBeaconPolicy 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,1),(offerBeacon,1),("other",1)]
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
                  , PV2.singleton beaconCurrencySymbol "other" 1
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Asset1 and asset2 are out of order.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum{ asset1Id = fst offer
                                               , asset1Name = snd offer
                                               , asset2Id = fst ask
                                               , asset2Name = snd ask
                                               }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a zero asset2 swap price.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 0 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a negative asset2 swap price numerator.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio (-1) 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a zero asset2 swap price denominator.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeAsset2Id = fst offer
        , unsafeAsset2Name = snd offer
        , unsafeAsset2Beacon = offerBeacon
        , unsafeAsset1Id = fst ask
        , unsafeAsset1Name = snd ask
        , unsafeAsset1Beacon = askBeacon
        , unsafeAsset2Price = (1,0)
        , unsafeAsset1Price = (1,1)
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a negative asset2 swap price denominator.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeAsset2Id = fst offer
        , unsafeAsset2Name = snd offer
        , unsafeAsset2Beacon = offerBeacon
        , unsafeAsset1Id = fst ask
        , unsafeAsset1Name = snd ask
        , unsafeAsset1Beacon = askBeacon
        , unsafeAsset2Price = (1,-1)
        , unsafeAsset1Price = (1,1)
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a zero asset1 swap price.
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

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 0 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a negative asset1 swap price numerator.
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

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio (-1) 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a zero asset1 swap price denominator.
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

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeAsset2Id = fst offer
        , unsafeAsset2Name = snd offer
        , unsafeAsset2Beacon = offerBeacon
        , unsafeAsset1Id = fst ask
        , unsafeAsset1Name = snd ask
        , unsafeAsset1Beacon = askBeacon
        , unsafeAsset2Price = (1,1)
        , unsafeAsset1Price = (1,0)
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | Swap datum has a negative asset1 swap price denominator.
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

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeAsset2Id = fst offer
        , unsafeAsset2Name = snd offer
        , unsafeAsset2Beacon = offerBeacon
        , unsafeAsset1Id = fst ask
        , unsafeAsset1Name = snd ask
        , unsafeAsset1Beacon = askBeacon
        , unsafeAsset2Price = (1,1)
        , unsafeAsset1Price = (1,-1)
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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap UTxO has an extraneous asset.
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

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  , PV2.singleton testTokenSymbol "TestToken2" 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap datum is a datum hash.
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

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = OutputDatumHash $ datumHash swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | The swap UTxO does not have a datum.
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer 10
                  ]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the pair beacons are mixed
-- up in the outputs.
failureTest43 :: MonadEmulator m => m ()
failureTest43 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton offer1 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton offer2 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the asset2 beacons are mixed
-- up in the outputs.
failureTest44 :: MonadEmulator m => m ()
failureTest44 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton offer1 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton offer2 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the asset1 beacons are mixed
-- up in the outputs.
failureTest45 :: MonadEmulator m => m ()
failureTest45 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (testTokenSymbol,"TestToken4")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = (testTokenSymbol,"TestToken5")
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = (testTokenSymbol,"TestToken6")
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton offer1 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton offer2 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs for different trading pairs, the datums are mixed
-- up.
failureTest46 :: MonadEmulator m => m ()
failureTest46 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer1 10
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
                  , uncurry PV2.singleton offer2 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs, the first output is invalid. This test and the next one
-- are meant to explicitly check that the order of the outputs does not impact the transaction's
-- validity.
failureTest47 :: MonadEmulator m => m ()
failureTest47 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer2 10
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
                  , uncurry PV2.singleton offer2 10
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
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

-- | When creating multiple swap UTxOs, the second output is invalid. This test and the previous 
-- one are meant to explicitly check that the order of the outputs does not impact the 
-- transaction's validity.
failureTest48 :: MonadEmulator m => m ()
failureTest48 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap1 Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = ask1 -- same as swap1
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

      -- Swap3 Info
      offer3 = (testTokenSymbol,"TestToken3")
      ask3 = ask1 -- same as swap1
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genAssetBeaconName offer3
      askBeacon3 = genAssetBeaconName ask3
      swapDatum3 = 
        genSwapDatum (offer3,ask3) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                  , uncurry PV2.singleton offer1 10
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
                  , uncurry PV2.singleton offer1 10
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
                  , uncurry PV2.singleton offer3 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3
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
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1 1_000_000) Nothing

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
                , uncurry PV2.singleton offer 10
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
      offers = map (testTokenSymbol,) $ drop 40 assetNames
      asks = map (testTokenSymbol,) $ take 40 assetNames
      pairs = zip offers asks
      datums = take numberCreated $
        flip map pairs $ \(offer,ask) -> 
          genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 1 1) Nothing

      sampleOutputs = flip map datums $ \datum@SwapDatum{..} ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol asset1Beacon 1
                , PV2.singleton beaconCurrencySymbol asset2Beacon 1
                , PV2.singleton asset2Id asset2Name 10
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
                  , (asset1Beacon,1)
                  , (asset2Beacon,1)
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
    , mustSucceed "regressionTest5" regressionTest5

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
        "Two-way swaps must have exactly three kinds of beacons" 
        failureTest4
    , scriptMustFailWithError "failureTest5" 
        "UTxO has wrong beacons" 
        failureTest5
    , scriptMustFailWithError "failureTest6" 
        "UTxO has wrong beacons" 
        failureTest6
    , scriptMustFailWithError "failureTest7" 
        "Wrong asset2_beacon" 
        failureTest7
    , scriptMustFailWithError "failureTest8" 
        "UTxO has wrong beacons" 
        failureTest8
    , scriptMustFailWithError "failureTest9" 
        "Two-way swaps must have exactly three kinds of beacons" 
        failureTest9
    , scriptMustFailWithError "failureTest10" 
        "UTxO has wrong beacons" 
        failureTest10
    , scriptMustFailWithError "failureTest11" 
        "UTxO has wrong beacons" 
        failureTest11
    , scriptMustFailWithError "failureTest12" 
        "Wrong asset1_beacon" 
        failureTest12
    , scriptMustFailWithError "failureTest13" 
        "UTxO has wrong beacons" 
        failureTest13
    , scriptMustFailWithError "failureTest14" 
        "Two-way swaps must have exactly three kinds of beacons" 
        failureTest14
    , scriptMustFailWithError "failureTest15" 
        "UTxO has wrong beacons" 
        failureTest15
    , scriptMustFailWithError "failureTest16" 
        "Two-way swaps must have exactly three kinds of beacons" 
        failureTest16
    , scriptMustFailWithError "failureTest17" 
        "Two-way swaps must have exactly three kinds of beacons" 
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
        "Two-way swaps must have exactly three kinds of beacons" 
        failureTest29
    , scriptMustFailWithError "failureTest30" 
        "UTxO has wrong beacons" 
        failureTest30
    , scriptMustFailWithError "failureTest31" 
        "Asset1 must be less than asset2" 
        failureTest31
    , scriptMustFailWithError "failureTest32" 
        "asset2_price numerator not > 0" 
        failureTest32
    , scriptMustFailWithError "failureTest33" 
        "asset2_price numerator not > 0" 
        failureTest33
    , scriptMustFailWithError "failureTest34" 
        "asset2_price denominator not > 0" 
        failureTest34
    , scriptMustFailWithError "failureTest35" 
        "asset2_price denominator not > 0" 
        failureTest35
    , scriptMustFailWithError "failureTest36" 
        "asset1_price numerator not > 0" 
        failureTest36
    , scriptMustFailWithError "failureTest37" 
        "asset1_price numerator not > 0" 
        failureTest37
    , scriptMustFailWithError "failureTest38" 
        "asset1_price denominator not > 0" 
        failureTest38
    , scriptMustFailWithError "failureTest39" 
        "asset1_price denominator not > 0" 
        failureTest39
    , scriptMustFailWithError "failureTest40" 
        "No extraneous assets allowed in the UTxO" 
        failureTest40
    , scriptMustFailWithError "failureTest41" 
        "All swap datums must be inline datums" 
        failureTest41
    , scriptMustFailWithError "failureTest42" 
        "All swap datums must be inline datums" 
        failureTest42
    , scriptMustFailWithError "failureTest43" 
        "UTxO has wrong beacons" 
        failureTest43
    , scriptMustFailWithError "failureTest44" 
        "UTxO has wrong beacons" 
        failureTest44
    , scriptMustFailWithError "failureTest45" 
        "UTxO has wrong beacons" 
        failureTest45
    , scriptMustFailWithError "failureTest46" 
        "UTxO has wrong beacons" 
        failureTest46
    , scriptMustFailWithError "failureTest47" 
        "No extraneous assets allowed in the UTxO" 
        failureTest47
    , scriptMustFailWithError "failureTest48" 
        "No extraneous assets allowed in the UTxO" 
        failureTest48

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 34
    , mustSucceed "benchTest2" $ benchTest2 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 35
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 25
    ]
