use cardano/transaction.{OutputReference}
use cardano/assets.{PolicyId,AssetName}
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
    asset1_price: Rational, // The swap price to take asset 1 as a fraction: Asset2/Asset1
    asset2_price: Rational, // The swap price to take asset 2 as a fraction: Asset1/Asset2
    prev_input: Option<OutputReference>,
    expiration: Option<Int>
  }
}

// Pre-images for the beacon token names: every beacon name is
// `sha2_256(cbor.serialise(swap_asset))`. The Data encoding tags each constructor and
// length-prefixes each field, so the encoding is injective: distinct assets/pairs can never
// produce the same pre-hash bytes, and beacon names cannot collide without a sha2_256 collision.
pub type SwapAsset {
  Asset(PolicyId, AssetName)
  // The pair must be sorted: asset1 < asset2 lexicographically. Enforced by generate_pair_beacon.
  SortedPair(PolicyId, AssetName, PolicyId, AssetName)
}

pub type SwapRedeemer {
  SpendWithMint
  SpendWithStake
  TakeAsset1
  TakeAsset2
}

// The beacon policy redeemer was dropped because it is no longer required. The script purpose now
// decides the dispatch, not the redeemer. For backwards compatibility with the prior version's ABI,
// the prior redeemer can continue to be used; it is just ignored by the smart contract.
