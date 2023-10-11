use aiken/transaction.{OutputReference}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/math/rational.{Rational}
use cardano_swaps/common/types.{AssetConfig}

pub type SwapDatum {
  SwapDatum { 
    beacon_id: PolicyId, // The beacon policy id for the associated offer asset.
    beacon_name: AssetName, // The asset name for the beacon.
    offer_id: PolicyId, // The policy id for the asset being offered.
    offer_name: AssetName, // The asset name for the asset being offered.
    ask_id: PolicyId, // The policy id for the asset being asked for.
    asked_name: AssetName, // The asset name for the asset being asked for.
    price: Rational, // The swap price as a fraction: Ask/Offer
    prev_input: Option<OutputReference>
  }
}

pub type SwapRedeemer {
  CloseOrUpdate
  Swap
}

pub type BeaconRedeemer {
  CreateSwap { ask_assets: List<AssetConfig> } // Mint a beacon for the target trading pairs.
  BurnBeacons
}

