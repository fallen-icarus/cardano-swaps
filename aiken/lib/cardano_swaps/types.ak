use aiken/transaction/value.{PolicyId,AssetName}
use aiken/math/rational.{Rational}

// An extra parameter so that every offer asset gets its own beacon.
pub type AssetConfig {
  asset_id: PolicyId,
  asset_name: AssetName
}

pub type SwapDatum {
  SwapDatum { 
    beacon_id: PolicyId, // The beacon policy id for the associated offer asset.
    beacon_name: AssetName, // The asset name for the beacon.
    offer_id: PolicyId, // The policy id for the asset being offered.
    offer_name: AssetName, // The asset name for the asset being offered.
    ask_id: PolicyId, // The policy id for the asset being asked for.
    asked_name: AssetName, // The asset name for the asset being asked for.
    price: Rational // The swap price as a fraction: Ask/Offer
  }
}

pub type SwapRedeemer {
  CloseOrUpdate
  Swap
}

pub type BeaconRedeemer {
  MintBeacon { ask_asset: AssetConfig } // Mint a beacon for the target trading pair.
  BurnBeacons
}