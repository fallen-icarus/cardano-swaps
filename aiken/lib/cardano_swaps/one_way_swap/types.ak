use aiken/transaction.{OutputReference}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/math/rational.{Rational}

pub type SwapDatum {
  SwapDatum { 
    beacon_id: PolicyId, // The beacon policy id for two-way swaps.
    pair_beacon: AssetName, // The asset name for the beacon for this trading pair.
    offer_id: PolicyId, // The policy id for the offer asset.
    offer_name: AssetName, // The asset name for the offer asset.
    offer_beacon: AssetName, // The asset name for the offer beacon.
    ask_id: PolicyId, // The policy id for the ask asset.
    ask_name: AssetName, // The asset name for the ask asset.
    ask_beacon: AssetName, // The asset name for the ask beacon.
    swap_price: Rational, // The swap price as a fraction: Ask/Offer.
    prev_input: Option<OutputReference>
  }
}

pub type SwapRedeemer {
  SpendWithMint
  SpendWithStake
  Swap
}

pub type BeaconRedeemer {
  CreateOrCloseSwaps // Minting execution.
  UpdateSwaps // Staking execution.
}
