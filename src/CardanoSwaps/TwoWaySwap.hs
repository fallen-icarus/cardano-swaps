{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module CardanoSwaps.TwoWaySwap
  (
    -- * On-Chain Data Types
    SwapDatum(..)
  , SwapRedeemer(..)
  , BeaconRedeemer(..)

    -- * Contracts
  , swapScript
  , swapValidator
  , swapValidatorHash
  , beaconScript
  , beaconMintingPolicy
  , beaconMintingPolicyHash
  , beaconCurrencySymbol

    -- * Generate Beacon Name
  , genBeaconName
  ) where

import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import qualified Data.Map as Map
import Plutus.Script.Utils.V2.Scripts
import qualified PlutusTx.Prelude as Plutus
import Data.List (sort)
import Data.Maybe (fromJust)

import CardanoSwaps.Utils
import CardanoSwaps.Blueprints

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data SwapDatum = SwapDatum
  { beaconId :: CurrencySymbol
  , beaconName :: TokenName
  , asset1Id :: CurrencySymbol
  , asset1Name :: TokenName
  , asset2Id :: CurrencySymbol
  , asset2Name :: TokenName
  , forwardPrice :: PlutusRational
  , reversePrice :: PlutusRational
  , prevInput :: Maybe TxOutRef
  }
  deriving (Generic,Show,Eq)

data SwapRedeemer
  = CloseOrUpdate
  | ForwardSwap
  | ReverseSwap
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateSwap [(AssetConfig,AssetConfig)] -- ^ The trading pairs.
  | BurnBeacons
  deriving (Generic,Show)

instance ToData BeaconRedeemer where
  toBuiltinData (CreateSwap xs) = dataToBuiltinData $
    Constr 0 
      [ Map $
          map (\((assetXid,assetXname),(assetYid,assetYname)) -> 
                ( Constr 0 [toData $ assetXid, toData $ assetXname]
                , Constr 0 [toData $ assetYid, toData $ assetYname]
                )
              ) 
              xs
      ]
     
  toBuiltinData BurnBeacons = dataToBuiltinData $
    Constr 1 []

instance FromData BeaconRedeemer where
  fromBuiltinData (BuiltinData (Constr 0 [Map pairs])) = 
    Just $ CreateSwap $
      map (\(Constr 0 [assetXid,assetXname],Constr 0 [assetYid,assetYname]) ->
              ( (fromJust $ fromData assetXid, fromJust $ fromData assetXname)
              , (fromJust $ fromData assetYid, fromJust $ fromData assetYname)
              )
          )
          pairs
    
  fromBuiltinData (BuiltinData (Constr 1 [])) = Just BurnBeacons
  fromBuiltinData _ = Nothing

PlutusTx.unstableMakeIsData ''SwapDatum
PlutusTx.unstableMakeIsData ''SwapRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
swapScript :: Ledger.Script
swapScript = parseScriptFromCBOR $ blueprints Map.! "two_way_swap.spend"

swapValidator :: Validator
swapValidator = Validator swapScript

swapValidatorHash :: ValidatorHash
swapValidatorHash = validatorHash swapValidator

beaconScript :: Ledger.Script
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "two_way_swap.mint")
    [toData swapValidatorHash]

beaconMintingPolicy :: MintingPolicy
beaconMintingPolicy = MintingPolicy beaconScript

beaconMintingPolicyHash :: MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash beaconMintingPolicy

beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol beaconMintingPolicy

-------------------------------------------------
-- Generate Beacon Name
-------------------------------------------------
-- | Generate the beacon asset name by hashing asset1 ++ asset2. The trading pair is first
-- sorted so that the beacon name is independent of the ordering.
genBeaconName :: (AssetConfig,AssetConfig) -> TokenName
genBeaconName (assetX,assetY) =
  let [((CurrencySymbol sym1),(TokenName name1)),((CurrencySymbol sym2),(TokenName name2))] =
        sort [assetX,assetY]
  in TokenName $ Plutus.sha2_256 $ sym1 <> name1 <> sym2 <> name2
