use aiken/transaction.{OutputReference}
use aiken/transaction/value.{PolicyId,AssetName}
use cardano_swaps/common/types.{Rational}

pub type SwapDatum {
  SwapDatum { 
    beacon_id: PolicyId, // The beacon policy id for two-way swaps.
    pair_beacon: AssetName, // The asset name for the beacon for this trading pair.
    asset1_id: PolicyId, // The policy id for the first asset.
    asset1_name: AssetName, // The asset name for the first asset.
    asset1_beacon: AssetName, // The asset name for asset1's offer beacon.
    asset2_id: PolicyId, // The policy id for the second asset.
    asset2_name: AssetName, // The asset name for the second asset.
    asset2_beacon: AssetName, // The asset name for asset2's offer beacon.
    forward_price: Rational, // The swap price as a fraction: Asset1/Asset2
    reverse_price: Rational, // The swap price as a fraction: Asset2/Asset1
    prev_input: Option<OutputReference>
  }
}

pub type SwapRedeemer {
  SpendWithMint
  SpendWithStake
  ForwardSwap
  ReverseSwap
}

pub type BeaconRedeemer {
  CreateOrCloseSwaps // Minting execution.
  UpdateSwaps // Staking execution.
}
