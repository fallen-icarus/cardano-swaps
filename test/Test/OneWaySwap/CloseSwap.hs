{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.CloseSwap
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3

    -- ** Scenarios that should fail
  , failureTest1
  , failureTest2
  , failureTest3
  , failureTest4
  , failureTest5
  , failureTest6
  , failureTest7
  , failureTest8
    
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
import Control.Monad (replicateM_,forM_)

import CardanoSwaps.OneWaySwap
import CardanoSwaps.Utils 

import Test.Prelude

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
              , outputValue = LV.lovelaceToValue 20_000_000
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript $ Just beaconScript
              }
          , Output
              { outputAddress = refScriptAddress
              , outputValue = LV.lovelaceToValue 22_000_000
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript $ Just swapScript
              }
          ]
      }

  (,) <$> txOutRefWithReferenceScript (scriptHash beaconScript)
      <*> txOutRefWithReferenceScript (scriptHash swapScript)

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
-- | Close a single swap UTxO.
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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,-1),(offerBeacon,-1),(askBeacon,-1)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Close multiple valid Swap UTxOs. All swaps are for the same trading pair.
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
  (mintRef,spendRef) <- initializeReferenceScripts 
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

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to close the swap UTxOs.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,-3),(offerBeacon,-3),(askBeacon,-3)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithMint)
            }
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Close multiple valid Swap UTxOs. All swaps are for unique trading pairs.
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
  (mintRef,spendRef) <- initializeReferenceScripts 
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

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to create the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,-1),(offerBeacon1,-1),(askBeacon1,-1) -- swap1
                  , (pairBeacon2,-1),(offerBeacon2,-1),(askBeacon2,-1) -- swap2
                  , (pairBeacon3,-1),(offerBeacon3,-1),(askBeacon3,-1) -- swap3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithMint)
            }
      , referenceInputs = [spendRef,mintRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | When closing a single swap UTxO, withdraw the pair beacon.
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
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,0),(offerBeacon,-1),(askBeacon,-1)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When closing a single swap UTxO, withdraw the offer beacon.
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
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,-1),(offerBeacon,0),(askBeacon,-1)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When closing a single swap UTxO, withdraw the ask beacon.
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
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,-1),(offerBeacon,-1),(askBeacon,0)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When closing a single swap UTxO, withdraw all of the beacons with the `SpendWithMint` redeemer.
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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,0),(offerBeacon,0),(askBeacon,0)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When closing a single swap UTxO, withdraw all of the beacons with the `SpendWithStake` 
-- redeemer. The beacon script is not executed as a staking script.
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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,0),(offerBeacon,0),(askBeacon,0)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer SpendWithStake)
              }
          ]
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When closing a single swap UTxO, withdraw all of the beacons with the `SpendWithStake` 
-- redeemer. The beacon script is executed as a staking script.
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
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum $ toRedeemer SpendWithStake
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

-- | When closing a single swap UTxO, mint a non-beacon asset with the minting policy.
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
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("other",1),(pairBeacon,-1),(offerBeacon,-1),(askBeacon,-1)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | When closing a single swap UTxO, the address' staking credential did not approve.
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
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

      -- Other Info
      otherWallet = Mock.knownMockWallet 2
      otherPersonalAddr = Mock.mockWalletAddress otherWallet
      otherPayPrivKey = Mock.paymentPrivateKey otherWallet
      otherPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash otherWallet

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
                  , uncurry PV2.singleton (unOfferAsset offer) 10
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
        , uncurry PV2.singleton (unOfferAsset offer) 10
        ]

  -- Try to close the swap UTxO.
  void $ transact otherPersonalAddr [swapAddress,refScriptAddress] [otherPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon,-1),(offerBeacon,-1),(askBeacon,-1)]
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
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [otherPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple swaps for the same trading pair.
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
      offer = OfferAsset (testTokenSymbol,"TestToken1")
      ask = AskAsset (adaSymbol,adaToken)
      pairBeacon = genPairBeaconName offer ask
      offerBeacon = genOfferBeaconName offer
      askBeacon = genAskBeaconName ask
      swapDatum = genSwapDatum offer ask (unsafeRatio 1_000_000 1) Nothing

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  replicateM_ 3 $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon,25)
                  , (offerBeacon,25)
                  , (askBeacon,25)
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
          
  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to close the swap UTxOs.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon,fromIntegral (-number))
                  , (offerBeacon,fromIntegral (-number))
                  , (askBeacon,fromIntegral (-number))
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer SpendWithMint
            }
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-- | Close multiple swap UTxOs for different trading pairs. 
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
      offers = map (\name -> OfferAsset (testTokenSymbol,name)) $ drop 60 assetNames
      asks = map (\name -> AskAsset (testTokenSymbol,name)) $ take 60 assetNames
      pairs = zip offers asks
      datums = 
        flip map pairs $ \(offer,ask) -> 
          genSwapDatum offer ask (unsafeRatio 1 1) Nothing

      sampleOutputs ds = flip map ds $ \datum@SwapDatum{..} ->
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
                    , (offerBeacon,1)
                    , (askBeacon,1)
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

  -- Try to close the swap UTxOs.
  void $ transact sellerPersonalAddr [swapAddress,refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap swaps $ \(_,Just SwapDatum{..}) ->
                  [ (pairBeacon,-1)
                  , (offerBeacon,-1)
                  , (askBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer SpendWithMint
            }
      , referenceInputs = [mintRef,spendRef]
      , extraKeyWitnesses = [sellerPubKey]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all swap close scenarios.
tests :: TestTree
tests =
  testGroup "Close Swap(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3

      -- Failure Tests
    , scriptMustFailWithError "failureTest1" 
        "One-way swaps must have exactly three kinds of beacons"
        failureTest1
    , scriptMustFailWithError "failureTest2" 
        "One-way swaps must have exactly three kinds of beacons"
        failureTest2
    , scriptMustFailWithError "failureTest3" 
        "One-way swaps must have exactly three kinds of beacons"
        failureTest3
    , scriptMustFailWithError "failureTest4" 
        "Beacon script not executed as minting policy"
        failureTest4
    , scriptMustFailWithError "failureTest5" 
        "Beacon script not executed as staking script"
        failureTest5
    , scriptMustFail "failureTest6" failureTest6
    , scriptMustFailWithError "failureTest7" 
        "One-way swaps must have exactly three kinds of beacons"
        failureTest7
    , scriptMustFailWithError "failureTest8" 
        "Staking credential did not approve"
        failureTest8

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 56
    , mustSucceed "benchTest2" $ benchTest2 56

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 57
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 57
    ]
