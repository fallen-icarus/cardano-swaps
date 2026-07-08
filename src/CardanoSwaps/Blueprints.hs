{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module CardanoSwaps.Blueprints
  ( -- * Blueprints
    blueprints
  ) where

import Data.Aeson
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.FileEmbed (embedFile)

-------------------------------------------------
-- Blueprints
-------------------------------------------------
newtype Blueprints = Blueprints (Map.Map String String)
  deriving (Show)

instance FromJSON Blueprints where
  parseJSON (Object o) =
    Blueprints . Map.fromList <$>
      (o .: "validators" >>=
        mapM (withObject "validator" $ \o' ->
          (,) . dropPurpose <$> o' .: "title" <*> o' .: "compiledCode"))
    where
      -- Newer aiken versions emit one entry per handler, with the purpose
      -- appended to the title (e.g. "one_way_swap.swap_script.spend"). The
      -- compiled code is identical for every purpose of a validator, so the
      -- purpose suffix is dropped to keep the old "module.validator" keys.
      dropPurpose :: String -> String
      dropPurpose s = case break (=='.') s of
        (m, '.':rest) -> m <> "." <> takeWhile (/='.') rest
        _ -> s
  parseJSON _ = mzero

-- | A map from validator "title" to "compiledCode" for the aiken/plutus.json file.
blueprints :: Map.Map String String
blueprints = 
  case decode $ LBS.fromStrict $(embedFile "aiken/plutus.json") of
    Nothing -> error "Failed to decode cardano-swaps' blueprint file"
    Just (Blueprints bs) -> bs
