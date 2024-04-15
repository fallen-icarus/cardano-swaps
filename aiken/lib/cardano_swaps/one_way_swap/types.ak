use aiken/transaction.{OutputReference}
use aiken/transaction/value.{AssetName, PolicyId}
use cardano_swaps/common/types.{Rational}

pub type SwapDatum {
  SwapDatum {
    // The beacon policy id for two-way swaps.
    beacon_id: PolicyId,
    // The asset name for the beacon for this trading pair.
    pair_beacon: AssetName,
    // The policy id for the offer asset.
    offer_id: PolicyId,
    // The asset name for the offer asset.
    offer_name: AssetName,
    // The asset name for the offer beacon.
    offer_beacon: AssetName,
    // The policy id for the ask asset.
    ask_id: PolicyId,
    // The asset name for the ask asset.
    ask_name: AssetName,
    // The asset name for the ask beacon.
    ask_beacon: AssetName,
    // The swap price as a fraction: Ask/Offer.
    swap_price: Rational,
    prev_input: Option<OutputReference>,
  }
}

pub type SwapRedeemer {
  SpendWithMint
  SpendWithStake
  Swap
}

pub type BeaconRedeemer {
  // Minting execution.
  CreateOrCloseSwaps
  // Staking execution.
  UpdateSwaps
}
