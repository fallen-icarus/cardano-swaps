use aiken/transaction.{OutputReference}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/math/rational.{Rational}
use cardano_swaps/common/types.{AssetConfig}

pub type SwapDatum {
  SwapDatum { 
    beacon_id: PolicyId, // The beacon policy id for the associated trading pair.
    beacon_name: AssetName, // The asset name for the beacon.
    asset1_id: PolicyId, // The policy id for the first asset.
    asset1_name: AssetName, // The asset name for the first asset.
    asset2_id: PolicyId, // The policy id for the second asset.
    asset2_name: AssetName, // The asset name for the second asset.
    forward_price: Rational, // The swap price as a fraction: Asset1/Asset2
    reverse_price: Rational, // The swap price as a fraction: Asset2/Asset1
    prev_input: Option<OutputReference>
  }
}

pub type SwapRedeemer {
  CloseOrUpdate
  ForwardSwap
  ReverseSwap
}

pub type BeaconRedeemer {
  // Mint beacons for the target trading pairs.
  CreateSwap {trading_pairs: List<(AssetConfig,AssetConfig)>} 
  BurnBeacons
}
