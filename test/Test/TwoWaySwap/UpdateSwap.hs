{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.TwoWaySwap.UpdateSwap
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

    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
    
    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Value.CardanoAPI as LV
import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.String (fromString)
import Control.Monad (replicateM_,forM_)

import CardanoSwaps.TwoWaySwap
import CardanoSwaps.Utils

import Test.Prelude
import Test.TwoWaySwap.UnsafeDatum

-------------------------------------------------
-- Initialize reference scripts.
-------------------------------------------------
initializeReferenceScripts :: MonadEmulator m => m (TxOutRef,TxOutRef)
initializeReferenceScripts = do
  let w1 = Mock.knownMockWallet 1
  
  void $ transact (Mock.mockWalletAddress w1) [refScriptAddress] [Mock.paymentPrivateKey w1] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = refScriptAddress
              , outputValue = LV.lovelaceToValue 22_000_000
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript $ Just beaconScript
              }
          , Output
              { outputAddress = refScriptAddress
              , outputValue = LV.lovelaceToValue 24_000_000
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript $ Just swapScript
              }
          ]
      , certificates =
          [ Certificate
              { certificateCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , certificateWitness =
                  StakeWithPlutusScript
                    (toVersionedLedgerScript beaconScript)
                    (toRedeemer UpdateSwaps)
              , certificateAction = Register
              }
          ]
      }

  (,) <$> txOutRefWithReferenceScript (scriptHash beaconScript)
      <*> txOutRefWithReferenceScript (scriptHash swapScript)

-------------------------------------------------
-- Mint Test Tokens
-------------------------------------------------
mintTestTokens :: MonadEmulator m => Mock.MockWallet -> Lovelace -> [(TokenName,Integer)] -> m ()
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
-- | Update the price of a single swap UTxO.
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
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Price = unsafeRatio 10 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Update the price of multiple swap UTxOs.
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
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  let updatedOuts = flip map swaps $ \(ref, Just datum) ->
        Output
          { outputAddress = swapAddress
          , outputValue = utxoValue 3_000_000 $ mconcat
              [ PV2.singleton beaconCurrencySymbol pairBeacon 1
              , PV2.singleton beaconCurrencySymbol offerBeacon 1
              , PV2.singleton beaconCurrencySymbol askBeacon 1
              ]
          , outputDatum = OutputDatum $ toDatum datum{asset1Price = unsafeRatio 10 1}
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Try to update the swap prices.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(ref,_) ->
          Input
            { inputId = ref
            , inputWitness =
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
            }
      , outputs = updatedOuts
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Convert a swap UTxO to a different trading pair.
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
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

      -- Swap2 Info
      offer2 = (testTokenSymbol,"TestToken2")
      ask2 = (adaSymbol,adaToken)
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genAssetBeaconName offer2
      askBeacon2 = genAssetBeaconName ask2
      swapDatum2 = 
        genSwapDatum (offer2,ask2) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)]
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
        , PV2.singleton beaconCurrencySymbol offerBeacon1 1
        , PV2.singleton beaconCurrencySymbol askBeacon1 1
        ]

  -- Try to convert the swap's trading pair.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1) -- old swap
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- new swap
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithMint)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{asset1Price = unsafeRatio 10 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Move a swap to a new swap address.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      newSellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash $ Mock.knownMockWallet 2
      swapAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript)
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)
      swapAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript)
                    (Just $ PV2.StakingHash
                          $ PV2.PubKeyCredential
                          $ LA.unPaymentPubKeyHash
                          $ Mock.paymentPubKeyHash
                          $ Mock.knownMockWallet 2)

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
              { outputAddress = swapAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price.
  void $ transact sellerPersonalAddr [swapAddress1,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress2
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey, newSellerPubKey]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | When updating a swap with the beacon script executed as a staking script, the new swap
-- datum has a zero asset1 swap price.
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
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum =
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Price = unsafeRatio 0 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When updating a swap with the beacon script executed as a staking script, the new swap
-- datum has a zero denominator for the asset1 swap price.
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
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum =
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing
      updatedDatum = UnsafeDatum
        { unsafeBeaconId = beaconCurrencySymbol
        , unsafePairBeacon = pairBeacon
        , unsafeAsset1Id = fst ask
        , unsafeAsset1Name = snd ask
        , unsafeAsset1Beacon = askBeacon
        , unsafeAsset2Id = fst offer
        , unsafeAsset2Name = snd offer
        , unsafeAsset2Beacon = offerBeacon
        , unsafeAsset1Price = (10,0)
        , unsafeAsset2Price = (10,1)
        , unsafePrevInput = Nothing
        , unsafeExpiration = Nothing
        }

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum updatedDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When updating a swap with the beacon script executed as a staking script, the updated
-- swap UTxO is stored at a swap address without a staking credential.
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
      swapAddressWithoutStaking = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) Nothing

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum =
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to move the swap UTxO to a swap address without staking.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddressWithoutStaking
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Price = unsafeRatio 10 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When updating a swap with the beacon script executed as a staking script, the updated
-- swap UTxO is stored at a non-swap address.
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
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to move the swap UTxO to a non-swap address.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = sellerPersonalAddr
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Price = unsafeRatio 10 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When updating a swap with the beacon script executed as a staking script, the new swap
-- datum has the wrong asset2 name; the beacons and the rest of the datum still correspond to
-- the original trading pair.
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
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap datum with the wrong asset2 name.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset2Name = "TestToken2"}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When updating a swap with the beacon script executed as a staking script, the updated
-- swap UTxO has an extraneous asset.
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
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum =
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000),("TestToken2",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price and add an extraneous asset to the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , PV2.singleton testTokenSymbol "TestToken2" 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Price = unsafeRatio 10 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When updating a swap with the beacon script executed as a staking script, the address'
-- staking credential did not approve.
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

      -- Other Wallet Info
      otherWallet = Mock.knownMockWallet 2
      otherPersonalAddr = Mock.mockWalletAddress otherWallet
      otherPayPrivKey = Mock.paymentPrivateKey otherWallet
      otherPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash otherWallet

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum =
        genSwapDatum (offer,ask) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <-
    txOutRefWithValue $
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price without the staking credential's approval.
  void $ transact otherPersonalAddr [swapAddress,refScriptAddress] [otherPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum{asset1Price = unsafeRatio 10 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [otherPubKey]
      }

-- | New owner did not approve.
failureTest8 :: MonadEmulator m => m ()
failureTest8 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      -- newSellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash $ Mock.knownMockWallet 2
      swapAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript)
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)
      swapAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript)
                    (Just $ PV2.StakingHash
                          $ PV2.PubKeyCredential
                          $ LA.unPaymentPubKeyHash
                          $ Mock.paymentPubKeyHash
                          $ Mock.knownMockWallet 2)

      -- Swap Info
      offer = (testTokenSymbol,"TestToken1")
      ask = (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genAssetBeaconName offer
      askBeacon = genAssetBeaconName ask
      swapDatum = 
        genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
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
              { outputAddress = swapAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon 1
        , PV2.singleton beaconCurrencySymbol offerBeacon 1
        , PV2.singleton beaconCurrencySymbol askBeacon 1
        ]

  -- Try to update the swap price.
  void $ transact sellerPersonalAddr [swapAddress1,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness =
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress2
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Update the swap prices for multiple swap UTxOs. All swaps are for the same trading pair.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer1 = (testTokenSymbol,"TestToken1")
      ask1 = (adaSymbol,adaToken)
      pairBeacon1= genPairBeaconName offer1 ask1
      offerBeacon1 = genAssetBeaconName offer1
      askBeacon1 = genAssetBeaconName ask1
      swapDatum1 = 
        genSwapDatum (offer1,ask1) (unsafeRatio 1_000_000 1) (unsafeRatio 1_000_000 1) Nothing Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  replicateM_ 3 $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,25)
                  , (offerBeacon1,25)
                  , (askBeacon1,25)
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs = replicate 25 $
          Output
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
      , referenceInputs = [mintRef]
      }
          
  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to update the prices for the swap UTxOs.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness =
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer SpendWithStake
            }
      , outputs = flip map swaps (\(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol asset1Beacon 1
                , PV2.singleton beaconCurrencySymbol asset2Beacon 1
                , PV2.singleton asset1Id asset1Name 10
                ]
            , outputDatum = OutputDatum $ toDatum datum{asset1Price = unsafeRatio 10 1}
            , outputReferenceScript = toReferenceScript Nothing
            })
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Update the swap prices for multiple swap UTxOs. All swaps are for different trading pairs.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..120]

      -- Swap Info
      offers = map (testTokenSymbol,) $ drop 60 assetNames
      asks = map (testTokenSymbol,) $ take 60 assetNames
      pairs = zip offers asks
      datums = 
        flip map pairs $ \(offer,ask) -> 
          genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 1 1) Nothing Nothing

      sampleOutputs ds = flip map ds $ \datum@SwapDatum{..} ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol asset1Beacon 1
                , PV2.singleton beaconCurrencySymbol asset2Beacon 1
                , PV2.singleton asset1Id asset1Name 10
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 100_000_000 $ zip assetNames (repeat 1000)

  -- Create the swap UTxO.
  forM_ (grouped 20 datums) $ \ds -> 
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \SwapDatum{..} ->
                    [ (pairBeacon,1)
                    , (asset1Beacon,1)
                    , (asset2Beacon,1)
                    ]
                , mintRedeemer = toRedeemer CreateOrCloseSwaps
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just mintRef
                }
            ]
        , outputs = sampleOutputs ds
        , referenceInputs = [mintRef]
        }

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to update the swap prices.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness =
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer SpendWithStake
            }
      , outputs = flip map swaps (\(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol asset1Beacon 1
                , PV2.singleton beaconCurrencySymbol asset2Beacon 1
                , PV2.singleton asset1Id asset1Name 10
                ]
            , outputDatum = OutputDatum $ toDatum datum{asset1Price = unsafeRatio 10 1}
            , outputReferenceScript = toReferenceScript Nothing
            })
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash beaconScript
              , withdrawalAmount = 0
              , withdrawalWitness =
                  StakeWithPlutusReference mintRef $ toRedeemer UpdateSwaps
              }
          ]
      , referenceInputs = [spendRef,mintRef]
      , extraKeyWitnesses = [sellerPubKey]
      }
  
-- | Convert swaps to different trading pairs.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 number = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..120]

      -- Swap Info
      offers = map (testTokenSymbol,) $ drop 60 assetNames
      asks = map (testTokenSymbol,) $ take 60 assetNames
      pairs = zip offers asks
      datums = 
        flip map pairs $ \(offer,ask) -> 
          genSwapDatum (offer,ask) (unsafeRatio 1 1) (unsafeRatio 1 1) Nothing Nothing

      beforeDatums = take 40 datums
      afterDatums = drop 40 datums

      sampleOutputs ds = flip map ds $ \datum@SwapDatum{..} ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol asset1Beacon 1
                , PV2.singleton beaconCurrencySymbol asset2Beacon 1
                , PV2.singleton asset1Id asset1Name 10
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 $ zip assetNames (repeat 1000)


  -- Create the swap UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \SwapDatum{..} ->
                    [ (pairBeacon,1)
                    , (asset1Beacon,1)
                    , (asset2Beacon,1)
                    ]
                , mintRedeemer = toRedeemer CreateOrCloseSwaps
                , mintPolicy = toVersionedMintingPolicy beaconScript
                , mintReference = Just mintRef
                }
            ]
        , outputs = sampleOutputs ds
        , referenceInputs = [mintRef]
        }

  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  let sampleBurns = 
        zipWith (\(_,Just beforeDatum) afterDatum ->
                  TokenMint 
                    { mintPolicy = toVersionedMintingPolicy beaconScript
                    , mintRedeemer = toRedeemer CreateOrCloseSwaps 
                    , mintTokens = 
                        [ (pairBeacon beforeDatum,-1)
                        , (asset1Beacon beforeDatum,-1)
                        , (asset2Beacon beforeDatum,-1)
                        , (pairBeacon afterDatum,1)
                        , (asset1Beacon afterDatum,1)
                        , (asset2Beacon afterDatum,1)
                        ]
                    , mintReference = Just mintRef
                    })
                swaps
                afterDatums

  -- Try to update the swap prices.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns
      , inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer SpendWithMint
            }
      , outputs = flip map (take number afterDatums) $ \datum@SwapDatum{..} ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol asset1Beacon 1
                , PV2.singleton beaconCurrencySymbol asset2Beacon 1
                , PV2.singleton asset1Id asset1Name 10
                ]
            , outputDatum = OutputDatum $ toDatum datum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [spendRef,mintRef]
      , extraKeyWitnesses = [sellerPubKey]
      }
  
-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all swap close scenarios.
tests :: TestTree
tests =
  testGroup "Update Swap(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3
    , mustSucceed "regressionTest4" regressionTest4

      -- Failure Tests
    , scriptMustFailWithError "failureTest1"
        "asset1_price numerator not > 0"
        failureTest1
    , scriptMustFailWithError "failureTest2"
        "asset1_price denominator not > 0"
        failureTest2
    , scriptMustFail "failureTest3" failureTest3
    , scriptMustFail "failureTest4" failureTest4
    , scriptMustFailWithError "failureTest5"
        "Wrong pair_beacon"
        failureTest5
    , scriptMustFailWithError "failureTest6"
        "No extraneous assets allowed in the UTxO"
        failureTest6
    , scriptMustFailWithError "failureTest7"
        "Staking credential did not approve"
        failureTest7
    , scriptMustFailWithError "failureTest8"
        "Staking credential did not approve"
        failureTest8

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 33
    , mustSucceed "benchTest2" $ benchTest2 22
    , mustSucceed "benchTest3" $ benchTest3 16

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 34
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 23
    , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 17
    ]
