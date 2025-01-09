{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OneWaySwap.Swap
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5
  , regressionTest6
  , regressionTest7
  , regressionTest8
  , regressionTest9

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
import Control.Monad (replicateM_,forM_,forM)

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
-- | Swap with a single swap UTxO.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple valid swap UTxOs. All swaps are for the same trading pair and are located
-- at the same swap address.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon1,3),(offerBeacon1,3),(askBeacon1,3)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs = replicate 3 $
          Output
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
          
      , referenceInputs = [mintRef]
      }

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(ref,_) ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs = flip map swaps $ \(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , PV2.singleton offerId offerName 5
                , PV2.singleton askId askName 5_000_000
                ]
            , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple valid swap UTxOs. All swaps are for the same trading pair and are located
-- at different swap addresses.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let -- Swap Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 4
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs. Returns the actual swap addresses used.
  swapAddrs <- forM [1..3] $ \i -> do
      let -- Seller Info
          sellerWallet = Mock.knownMockWallet i
          sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
          sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
          sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
          swapAddress = toCardanoApiAddress $
            PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                        (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]

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
                      , uncurry PV2.singleton (unOfferAsset offer1) 10
                      ]
                  , outputDatum = OutputDatum $ toDatum swapDatum1
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
          , referenceInputs = [mintRef]
          }

      return swapAddress

  swaps <- mapM (txOutRefsAndDatumsAtAddress @SwapDatum) swapAddrs

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr ([refScriptAddress] <> swapAddrs) [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap swaps $ \ss ->
          flip map ss $ \(ref,_) ->
            Input
              { inputId = ref
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
      , outputs = flip concatMap (zip swaps swapAddrs) $ \(ss,swapAddress) ->
          flip map ss $ \(ref,Just datum@SwapDatum{..}) ->
            Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , PV2.singleton offerId offerName 5
                  , PV2.singleton askId askName 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
              , outputReferenceScript = toReferenceScript Nothing
              }
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple valid swap UTxOs. All swaps are for different trading pairs and are located
-- at the same swap address.
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
      ask2 = AskAsset (adaSymbol,adaToken)
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap1 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = AskAsset (adaSymbol,adaToken)
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens sellerWallet 10_000_000 [("TestToken2",1000)]
  mintTestTokens sellerWallet 10_000_000 [("TestToken3",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap 3
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

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(ref,_) ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs = flip map swaps $ \(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , PV2.singleton offerId offerName 5
                , PV2.singleton askId askName 5_000_000
                ]
            , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple valid swap UTxOs. All swaps are for different trading pairs and are located
-- at different swap addresses.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
  let -- Buyer Info
      buyerWallet = Mock.knownMockWallet 4
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs. Returns the actual swap addresses used.
  swapAddrs <- forM [1..3] $ \i -> do
      let -- Seller Info
          sellerWallet = Mock.knownMockWallet i
          sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
          sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
          sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
          swapAddress = toCardanoApiAddress $
            PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                        (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

          -- Swap Info
          offer1 = OfferAsset (testTokenSymbol,fromString $ "TestToken" <> show i)
          ask1 = AskAsset (adaSymbol,adaToken)
          pairBeacon1 = genPairBeaconName offer1 ask1
          offerBeacon1 = genOfferBeaconName offer1
          askBeacon1 = genAskBeaconName ask1
          swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      mintTestTokens sellerWallet 10_000_000 [(snd $ unOfferAsset offer1,1000)]

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
                      , uncurry PV2.singleton (unOfferAsset offer1) 10
                      ]
                  , outputDatum = OutputDatum $ toDatum swapDatum1
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
          , referenceInputs = [mintRef]
          }

      return swapAddress

  swaps <- mapM (txOutRefsAndDatumsAtAddress @SwapDatum) swapAddrs

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr ([refScriptAddress] <> swapAddrs) [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap swaps $ \ss ->
          flip map ss $ \(ref,_) ->
            Input
              { inputId = ref
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
      , outputs = flip concatMap (zip swaps swapAddrs) $ \(ss,swapAddress) ->
          flip map ss $ \(ref,Just datum@SwapDatum{..}) ->
            Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , PV2.singleton offerId offerName 5
                  , PV2.singleton askId askName 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
              , outputReferenceScript = toReferenceScript Nothing
              }
      , referenceInputs = [spendRef]
      }

-- | When ADA is not part of the trading pair, ADA can still be deposited in case the minimum
-- required ADA amount for the UTxO increases.
regressionTest6 :: MonadEmulator m => m ()
regressionTest6 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (testTokenSymbol,"TestToken2")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000),("TestToken2",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000),("TestToken2",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple valid swap UTxOs. The swaps are for different trading pairs, are located
-- at the same address, and are composed together. The arbitrager just pays the transaction fee.
regressionTest7 :: MonadEmulator m => m ()
regressionTest7 = do
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
      ask1 = AskAsset (testTokenSymbol,"TestToken2")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (testTokenSymbol,"TestToken3")
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1 1) Nothing

      -- Swap1 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = AskAsset (testTokenSymbol,"TestToken1")
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap 3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
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
              , outputValue = utxoValue 4_000_000 $ mconcat
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
              , outputValue = utxoValue 4_000_000 $ mconcat
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

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(ref,_) ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs = flip map swaps $ \(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , PV2.singleton offerId offerName 5
                , PV2.singleton askId askName 5
                ]
            , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple valid swap UTxOs. All swaps are for different trading pairs and are located
-- at different swap addresses. The swaps are composed together. The arbitrager just pays the
-- transaction fee.
regressionTest8 :: MonadEmulator m => m ()
regressionTest8 = do
  let -- Buyer Info
      buyerWallet = Mock.knownMockWallet 4
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs. Returns the actual swap addresses used.
  let users = [1..3]
      numberOfUsers = length users
  swapAddrs <- forM users $ \i -> do
      let -- Seller Info
          sellerWallet = Mock.knownMockWallet $ fromIntegral i
          sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
          sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
          sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
          swapAddress = toCardanoApiAddress $
            PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                        (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

          -- Swap Info
          offer1 = OfferAsset (testTokenSymbol,fromString $ "TestToken" <> show i)
          ask1 = 
            if i + 1 == numberOfUsers 
            then AskAsset (testTokenSymbol,fromString $ "TestToken" <> show (i+1))
            else AskAsset ( testTokenSymbol
                          , fromString $ "TestToken" <> show ((i+1) `mod` numberOfUsers)
                          )
          pairBeacon1 = genPairBeaconName offer1 ask1
          offerBeacon1 = genOfferBeaconName offer1
          askBeacon1 = genAskBeaconName ask1
          swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      mintTestTokens sellerWallet 10_000_000 [(snd $ unOfferAsset offer1,1000)]

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
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                      , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                      , PV2.singleton beaconCurrencySymbol askBeacon1 1
                      , uncurry PV2.singleton (unOfferAsset offer1) 10
                      ]
                  , outputDatum = OutputDatum $ toDatum swapDatum1
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
          , referenceInputs = [mintRef]
          }

      return swapAddress

  swaps <- mapM (txOutRefsAndDatumsAtAddress @SwapDatum) swapAddrs

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr ([refScriptAddress] <> swapAddrs) [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap swaps $ \ss ->
          flip map ss $ \(ref,_) ->
            Input
              { inputId = ref
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
      , outputs = flip concatMap (zip swaps swapAddrs) $ \(ss,swapAddress) ->
          flip map ss $ \(ref,Just datum@SwapDatum{..}) ->
            Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon 1
                  , PV2.singleton beaconCurrencySymbol askBeacon 1
                  , PV2.singleton offerId offerName 5
                  , PV2.singleton askId askName 5
                  ]
              , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
              , outputReferenceScript = toReferenceScript Nothing
              }
      , referenceInputs = [spendRef]
      }

-- | Create a swap in the same transaction where another swap is made.
regressionTest9 :: MonadEmulator m => m ()
regressionTest9 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      sellerSwapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet
      buyerSwapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential buyerPubKey)

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
              { outputAddress = sellerSwapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [sellerSwapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = sellerSwapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = buyerSwapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{swapPrice = unsafeRatio 10_000_000 1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef,mintRef]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | Swap input does not have beacons.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxO.
  void $ transact sellerPersonalAddr [] [sellerPayPrivKey] $
    emptyTxParams
      { outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ uncurry PV2.singleton (unOfferAsset offer1) 10 ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      }

  swapRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ uncurry PV2.singleton (unOfferAsset offer1) 10 ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | Pair beacon withdrawn during swap.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 0
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | Offer beacon withdrawn during swap.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 0
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | Ask beacon withdrawn during swap.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 0
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | Not enough of the ask asset deposited during the swap.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 4_999_999
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The wrong asset is deposited during the swap.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken2",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , PV2.singleton testTokenSymbol "TestToken2" 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When the swap already has some of the ask asset, the offer asset is deposited and some
-- of the ask asset is taken. This is swapping in the wrong direction.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 15
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong beacon id.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ beaconId = "" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong pair beacon.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ pairBeacon = "" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong offer id.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ offerId = "" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong offer name.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ offerName = "" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong offer beacon.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ offerBeacon = "" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong ask id.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ askId = testTokenSymbol 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong ask name.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ askName = "TestToken" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong ask beacon.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ askBeacon = "" 
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong swap price.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{ swapPrice = unsafeRatio 0 1
                                                , prevInput = Just swapRef
                                                }
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The new swap output's datum has the wrong previous input.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum 
                            $ toDatum swapDatum1{prevInput = Just spendRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When swapping with multiple swaps for the same trading pair, the outputs are consolidated
-- into a single output.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(pairBeacon1,3),(offerBeacon1,3),(askBeacon1,3)]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs = replicate 3 $
          Output
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
          
      , referenceInputs = [mintRef]
      }

  swaps <- txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(ref,_) ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 9_000_000 $ mconcat $ flip foldMap swaps $ 
                  \(_,Just SwapDatum{..}) ->
                    [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                    , PV2.singleton beaconCurrencySymbol offerBeacon 1
                    , PV2.singleton beaconCurrencySymbol askBeacon 1
                    , PV2.singleton offerId offerName 5
                    , PV2.singleton askId askName 5_000_000
                    ]
              , outputDatum = OutputDatum 
                            $ toDatum 
                            $ (\(ref,Just datum) -> datum{prevInput = Just ref})
                            $ head swaps
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When swapping with multiple swaps for different trading pairs, the pair beacons are mixed
-- up in the outputs.
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

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (adaSymbol,adaToken)
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap1 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = AskAsset (adaSymbol,adaToken)
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens sellerWallet 10_000_000 [("TestToken2",1000)]
  mintTestTokens sellerWallet 10_000_000 [("TestToken3",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap 3
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

  swap1 <- 
    txOutRefWithValue $ utxoValue 3_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
      , PV2.singleton beaconCurrencySymbol offerBeacon1 1
      , PV2.singleton beaconCurrencySymbol askBeacon1 1
      , uncurry PV2.singleton (unOfferAsset offer1) 10
      ]
  swap2 <- 
    txOutRefWithValue $ utxoValue 3_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
      , PV2.singleton beaconCurrencySymbol offerBeacon2 1
      , PV2.singleton beaconCurrencySymbol askBeacon2 1
      , uncurry PV2.singleton (unOfferAsset offer2) 10
      ]
  swap3 <- 
    txOutRefWithValue $ utxoValue 3_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
      , PV2.singleton beaconCurrencySymbol offerBeacon3 1
      , PV2.singleton beaconCurrencySymbol askBeacon3 1
      , uncurry PV2.singleton (unOfferAsset offer3) 10
      ]

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map [swap1,swap2,swap3] $ \ref ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swap1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 5
                  , uncurry PV2.singleton (unAskAsset ask2) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{prevInput = Just swap2}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 5
                  , uncurry PV2.singleton (unAskAsset ask3) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3{prevInput = Just swap3}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When swapping with multiple swaps for different trading pairs, the offer beacons are mixed
-- up in the outputs.
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

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (adaSymbol,adaToken)
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1_000_000 1) Nothing

      -- Swap1 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = AskAsset (adaSymbol,adaToken)
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens sellerWallet 10_000_000 [("TestToken2",1000)]
  mintTestTokens sellerWallet 10_000_000 [("TestToken3",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap 3
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

  swap1 <- 
    txOutRefWithValue $ utxoValue 3_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
      , PV2.singleton beaconCurrencySymbol offerBeacon1 1
      , PV2.singleton beaconCurrencySymbol askBeacon1 1
      , uncurry PV2.singleton (unOfferAsset offer1) 10
      ]
  swap2 <- 
    txOutRefWithValue $ utxoValue 3_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
      , PV2.singleton beaconCurrencySymbol offerBeacon2 1
      , PV2.singleton beaconCurrencySymbol askBeacon2 1
      , uncurry PV2.singleton (unOfferAsset offer2) 10
      ]
  swap3 <- 
    txOutRefWithValue $ utxoValue 3_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
      , PV2.singleton beaconCurrencySymbol offerBeacon3 1
      , PV2.singleton beaconCurrencySymbol askBeacon3 1
      , uncurry PV2.singleton (unOfferAsset offer3) 10
      ]

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map [swap1,swap2,swap3] $ \ref ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swap1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 5
                  , uncurry PV2.singleton (unAskAsset ask2) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{prevInput = Just swap2}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 5
                  , uncurry PV2.singleton (unAskAsset ask3) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3{prevInput = Just swap3}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When swapping with multiple swaps for different trading pairs, the ask beacons are mixed
-- up in the outputs.
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

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (testTokenSymbol,"TestToken2")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (testTokenSymbol,"TestToken3")
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1 1) Nothing

      -- Swap1 Info
      offer3 = OfferAsset (testTokenSymbol,"TestToken3")
      ask3 = AskAsset (testTokenSymbol,"TestToken1")
      pairBeacon3 = genPairBeaconName offer3 ask3
      offerBeacon3 = genOfferBeaconName offer3
      askBeacon3 = genAskBeaconName ask3
      swapDatum3 = genSwapDatum offer3 ask3 (unsafeRatio 1 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  , (pairBeacon3,1),(offerBeacon3,1),(askBeacon3,1) -- swap 3
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
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
              , outputValue = utxoValue 4_000_000 $ mconcat
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
              , outputValue = utxoValue 4_000_000 $ mconcat
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

  swap1 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
      , PV2.singleton beaconCurrencySymbol offerBeacon1 1
      , PV2.singleton beaconCurrencySymbol askBeacon1 1
      , uncurry PV2.singleton (unOfferAsset offer1) 10
      ]
  swap2 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
      , PV2.singleton beaconCurrencySymbol offerBeacon2 1
      , PV2.singleton beaconCurrencySymbol askBeacon2 1
      , uncurry PV2.singleton (unOfferAsset offer2) 10
      ]
  swap3 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
      , PV2.singleton beaconCurrencySymbol offerBeacon3 1
      , PV2.singleton beaconCurrencySymbol askBeacon3 1
      , uncurry PV2.singleton (unOfferAsset offer3) 10
      ]

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map [swap1,swap2,swap3] $ \ref ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swap1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon3 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 5
                  , uncurry PV2.singleton (unAskAsset ask2) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{prevInput = Just swap2}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon3 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon3 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer3) 5
                  , uncurry PV2.singleton (unAskAsset ask3) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum3{prevInput = Just swap3}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The swap output's datum is a datum hash.
failureTest22 :: MonadEmulator m => m ()
failureTest22 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatumHash $ datumHash swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | The swap output does not have a datum.
failureTest23 :: MonadEmulator m => m ()
failureTest23 = do
  let -- Seller Info
      sellerWallet = Mock.knownMockWallet 1
      sellerPersonalAddr = Mock.mockWalletAddress sellerWallet
      sellerPayPrivKey = Mock.paymentPrivateKey sellerWallet
      sellerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash sellerWallet
      swapAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash swapScript) 
                    (Just $ PV2.StakingHash $ PV2.PubKeyCredential sellerPubKey)

      -- Swap Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When executing multiple swaps, the first swap output is invalid. This test and the next
-- one are meant to explicitly check that the order of the outputs does not impact the
-- transaction's validity.
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

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (testTokenSymbol,"TestToken2")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (testTokenSymbol,"TestToken3")
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
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
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
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

  swap1 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
      , PV2.singleton beaconCurrencySymbol offerBeacon1 1
      , PV2.singleton beaconCurrencySymbol askBeacon1 1
      , uncurry PV2.singleton (unOfferAsset offer1) 10
      ]
  swap2 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
      , PV2.singleton beaconCurrencySymbol offerBeacon2 1
      , PV2.singleton beaconCurrencySymbol askBeacon2 1
      , uncurry PV2.singleton (unOfferAsset offer2) 10
      ]

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map [swap1,swap2] $ \ref ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 4
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swap1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 5
                  , uncurry PV2.singleton (unAskAsset ask2) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{prevInput = Just swap2}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When executing multiple swaps, the second swap output is invalid. This test and the previous
-- one are meant to explicitly check that the order of the outputs does not impact the
-- transaction's validity.
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

      -- Swap1 Info
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (testTokenSymbol,"TestToken2")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      -- Swap2 Info
      offer2 = OfferAsset (testTokenSymbol,"TestToken2")
      ask2 = AskAsset (testTokenSymbol,"TestToken3")
      pairBeacon2 = genPairBeaconName offer2 ask2
      offerBeacon2 = genOfferBeaconName offer2
      askBeacon2 = genAskBeaconName ask2
      swapDatum2 = genSwapDatum offer2 ask2 (unsafeRatio 1 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]
  mintTestTokens buyerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Create the swap UTxOs.
  void $ transact sellerPersonalAddr [refScriptAddress] [sellerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (pairBeacon1,1),(offerBeacon1,1),(askBeacon1,1) -- swap 1
                  , (pairBeacon2,1),(offerBeacon2,1),(askBeacon2,1) -- swap 2
                  ]
              , mintRedeemer = toRedeemer CreateOrCloseSwaps
              , mintPolicy = toVersionedMintingPolicy beaconScript
              , mintReference = Just mintRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
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
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
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

  swap1 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
      , PV2.singleton beaconCurrencySymbol offerBeacon1 1
      , PV2.singleton beaconCurrencySymbol askBeacon1 1
      , uncurry PV2.singleton (unOfferAsset offer1) 10
      ]
  swap2 <- 
    txOutRefWithValue $ utxoValue 4_000_000 $ mconcat
      [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
      , PV2.singleton beaconCurrencySymbol offerBeacon2 1
      , PV2.singleton beaconCurrencySymbol askBeacon2 1
      , uncurry PV2.singleton (unOfferAsset offer2) 10
      ]

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map [swap1,swap2] $ \ref ->
          Input
            { inputId = ref
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
            }
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swap1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon2 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon2 1
                  , PV2.singleton beaconCurrencySymbol askBeacon2 1
                  , uncurry PV2.singleton (unOfferAsset offer2) 5
                  , uncurry PV2.singleton (unAskAsset ask2) 4
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum2{prevInput = Just swap2}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | When ADA is not part of the trading pair, withdraw some of it.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (testTokenSymbol,"TestToken2")
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken2",1000)]

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
              , outputValue = utxoValue 5_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [mintRef]
      }

  swapRef <- 
    txOutRefWithValue $ 
      utxoValue 5_000_000 $ mconcat
        [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
        , PV2.singleton beaconCurrencySymbol offerBeacon1 1
        , PV2.singleton beaconCurrencySymbol askBeacon1 1
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 1
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 1
                  , PV2.singleton beaconCurrencySymbol askBeacon1 1
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-- | All beacons withdrawn during swap.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

  -- Initialize scenario
  (mintRef,spendRef) <- initializeReferenceScripts 
  mintTestTokens sellerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens buyerWallet 10_000_000 [("TestToken1",1000)]

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
                  , uncurry PV2.singleton (unOfferAsset offer1) 10
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
        , uncurry PV2.singleton (unOfferAsset offer1) 10
        ]

  -- Try to swap with the swap UTxO.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = swapRef
              , inputWitness = 
                  SpendWithPlutusReference spendRef InlineDatum (toRedeemer Swap)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = swapAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton beaconCurrencySymbol pairBeacon1 0
                  , PV2.singleton beaconCurrencySymbol offerBeacon1 0
                  , PV2.singleton beaconCurrencySymbol askBeacon1 0
                  , uncurry PV2.singleton (unOfferAsset offer1) 5
                  , uncurry PV2.singleton (unAskAsset ask1) 5_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum swapDatum1{prevInput = Just swapRef}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [spendRef]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Swap multiple Swap UTxOs for the same trading pair and from the same address.
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
      offer1 = OfferAsset (testTokenSymbol,"TestToken1")
      ask1 = AskAsset (adaSymbol,adaToken)
      pairBeacon1 = genPairBeaconName offer1 ask1
      offerBeacon1 = genOfferBeaconName offer1
      askBeacon1 = genAskBeaconName ask1
      swapDatum1 = genSwapDatum offer1 ask1 (unsafeRatio 1_000_000 1) Nothing

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

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
                , uncurry PV2.singleton (unOfferAsset offer1) 10
                ]
            , outputDatum = OutputDatum $ toDatum swapDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [mintRef]
      }
          
  swaps <- take number <$> txOutRefsAndDatumsAtAddress @SwapDatum swapAddress

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer Swap
            }
      , outputs = flip map swaps $ \(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , PV2.singleton offerId offerName 5
                , PV2.singleton askId askName 5_000_000
                ]
            , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [spendRef]
      }

-- | Swap with multiple swap UTxOs for different trading pairs and from the same address.
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

      -- Buyer Info
      buyerWallet = Mock.knownMockWallet 2
      buyerPersonalAddr = Mock.mockWalletAddress buyerWallet
      buyerPayPrivKey = Mock.paymentPrivateKey buyerWallet
      buyerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash buyerWallet

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
            , outputValue = utxoValue 4_000_000 $ mconcat
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
  mintTestTokens buyerWallet 100_000_000 $ zip (map (snd . unAskAsset) asks) (repeat 1000)

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

  -- Try to swap with the swap UTxOs.
  void $ transact buyerPersonalAddr [swapAddress,refScriptAddress] [buyerPayPrivKey] $
    emptyTxParams
      { inputs = flip map swaps $ \(swapRef,_) ->
          Input
            { inputId = swapRef
            , inputWitness = 
                SpendWithPlutusReference spendRef InlineDatum $ toRedeemer Swap
            }
      , outputs = flip map swaps $ \(ref,Just datum@SwapDatum{..}) ->
          Output
            { outputAddress = swapAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton beaconCurrencySymbol pairBeacon 1
                , PV2.singleton beaconCurrencySymbol offerBeacon 1
                , PV2.singleton beaconCurrencySymbol askBeacon 1
                , PV2.singleton offerId offerName 5
                , PV2.singleton askId askName 5
                ]
            , outputDatum = OutputDatum $ toDatum datum{prevInput = Just ref}
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [spendRef]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all swap close scenarios.
tests :: TestTree
tests =
  testGroup "Swap(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3
    , mustSucceed "regressionTest4" regressionTest4
    , mustSucceed "regressionTest5" regressionTest5
    , mustSucceed "regressionTest6" regressionTest6
    , mustSucceed "regressionTest7" regressionTest7
    , mustSucceed "regressionTest8" regressionTest8
    , mustSucceed "regressionTest9" regressionTest9

      -- Failure Tests
    , scriptMustFailWithError "failureTest1"
        "UTxO has wrong beacons"
        failureTest1
    , scriptMustFailWithError "failureTest2"
        "UTxO has wrong beacons"
        failureTest2
    , scriptMustFailWithError "failureTest3"
        "UTxO has wrong beacons"
        failureTest3
    , scriptMustFailWithError "failureTest4"
        "UTxO has wrong beacons"
        failureTest4
    , scriptMustFailWithError "failureTest5"
        "Fail: offer_taken * price <= ask_given"
        failureTest5
    , scriptMustFailWithError "failureTest6"
        "No extraneous assets allowed in the UTxO"
        failureTest6
    , scriptMustFailWithError "failureTest7"
        "The ask asset cannot be taken from the swap"
        failureTest7
    , scriptMustFailWithError "failureTest8"
        "Corresponding swap output not found"
        failureTest8
    , scriptMustFailWithError "failureTest9"
        "Corresponding swap output not found"
        failureTest9
    , scriptMustFailWithError "failureTest10"
        "Corresponding swap output not found"
        failureTest10
    , scriptMustFailWithError "failureTest11"
        "Corresponding swap output not found"
        failureTest11
    , scriptMustFailWithError "failureTest12"
        "Corresponding swap output not found"
        failureTest12
    , scriptMustFailWithError "failureTest13"
        "Corresponding swap output not found"
        failureTest13
    , scriptMustFailWithError "failureTest14"
        "Corresponding swap output not found"
        failureTest14
    , scriptMustFailWithError "failureTest15"
        "Corresponding swap output not found"
        failureTest15
    , scriptMustFailWithError "failureTest16"
        "Corresponding swap output not found"
        failureTest16
    , scriptMustFailWithError "failureTest17"
        "Corresponding swap output not found"
        failureTest17
    , scriptMustFailWithError "failureTest18"
        "UTxO has wrong beacons"
        failureTest18
    , scriptMustFailWithError "failureTest19"
        "UTxO has wrong beacons"
        failureTest19
    , scriptMustFailWithError "failureTest20"
        "UTxO has wrong beacons"
        failureTest20
    , scriptMustFailWithError "failureTest21"
        "UTxO has wrong beacons"
        failureTest21
    , scriptMustFailWithError "failureTest22"
        "Corresponding swap output not found"
        failureTest22
    , scriptMustFailWithError "failureTest23"
        "Corresponding swap output not found"
        failureTest23
    , scriptMustFailWithError "failureTest24"
        "Fail: offer_taken * price <= ask_given"
        failureTest24
    , scriptMustFailWithError "failureTest25"
        "Fail: offer_taken * price <= ask_given"
        failureTest25
    , scriptMustFailWithError "failureTest26"
        "Ada can only be deposited"
        failureTest26
    , scriptMustFailWithError "failureTest27"
        "UTxO has wrong beacons"
        failureTest27

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 25
    , mustSucceed "benchTest2" $ benchTest1 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 26
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 25
    ]
