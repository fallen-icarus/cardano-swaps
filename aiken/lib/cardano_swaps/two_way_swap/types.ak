use aiken/transaction.{OutputReference}
use aiken/transaction/value.{AssetName, PolicyId}
use cardano_swaps/common/types.{Rational}

pub type SwapDatum {
  SwapDatum {
    // The beacon policy id for two-way swaps.
    beacon_id: PolicyId,
    // The asset name for the beacon for this trading pair.
    pair_beacon: AssetName,
    // The policy id for the first asset.
    asset1_id: PolicyId,
    // The asset name for the first asset.
    asset1_name: AssetName,
    // The asset name for asset1's offer beacon.
    asset1_beacon: AssetName,
    // The policy id for the second asset.
    asset2_id: PolicyId,
    // The asset name for the second asset.
    asset2_name: AssetName,
    // The asset name for asset2's offer beacon.
    asset2_beacon: AssetName,
    // The swap price to take asset 1 as a fraction: Asset2/Asset1
    asset1_price: Rational,
    // The swap price to take asset 2 as a fraction: Asset1/Asset2
    asset2_price: Rational,
    prev_input: Option<OutputReference>,
  }
}

pub type SwapRedeemer {
  SpendWithMint
  SpendWithStake
  TakeAsset1
  TakeAsset2
}

pub type BeaconRedeemer {
  // Minting execution.
  CreateOrCloseSwaps
  // Staking execution.
  UpdateSwaps
}
