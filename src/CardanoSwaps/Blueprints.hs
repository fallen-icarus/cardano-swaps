{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module CardanoSwaps.Blueprints
  ( -- * Blueprints
    blueprints
  ) where

import Data.Aeson as Aeson
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.FileEmbed

-------------------------------------------------
-- Blueprints
-------------------------------------------------
newtype Blueprints = Blueprints (Map.Map String String)
  deriving (Show)

instance FromJSON Blueprints where
  parseJSON (Object o) = 
    Blueprints . Map.fromList <$> 
      (o .: "validators" >>= mapM (\(Object o') -> (,) <$> o' .: "title" <*> o' .: "compiledCode"))
  parseJSON _ = mzero

blueprints :: Map.Map String String
blueprints = 
  case decode $ LBS.fromStrict $(embedFile "aiken/plutus.json") of
    Nothing -> error "Failed to decode cardano-swaps' blueprint file"
    Just (Blueprints bs) -> bs
