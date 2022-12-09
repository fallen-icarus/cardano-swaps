module CLI.Query
(
  Network (..),
  BlockfrostApiKey (..),
) where

data Network
  -- | Uses the Koios REST api. 
  = Mainnet
  -- | Koios REST api does not support the PreProduction Testnet.
  --   Until they do, Blockfrost will be used for querying the PreProduction Testnet.
  | PreProdTestnet BlockfrostApiKey

newtype BlockfrostApiKey = BlockfrostApiKey String