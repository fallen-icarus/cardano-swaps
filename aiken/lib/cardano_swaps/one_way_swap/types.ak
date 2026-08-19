use cardano/transaction.{OutputReference}
use cardano/assets.{PolicyId,AssetName}
use cardano_swaps/common/types.{Rational}

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
    prev_input: Option<OutputReference>,
    expiration: Option<Int>
  }
}

// Pre-images for the beacon token names: every beacon name is
// `sha2_256(cbor.serialise(swap_asset))`. The Data encoding tags each constructor and
// length-prefixes each field, so the encoding is injective: distinct assets/pairs can never
// produce the same pre-hash bytes, and beacon names cannot collide without a sha2_256 collision.
pub type SwapAsset {
  OfferAsset(PolicyId, AssetName)
  AskAsset(PolicyId, AssetName)
  // The offer asset must come first: (offer_id, offer_name, ask_id, ask_name).
  TradingPair(PolicyId, AssetName, PolicyId, AssetName)
}

pub type SwapRedeemer {
  SpendWithMint
  SpendWithStake
  Swap
}

// The beacon policy redeemer was dropped because it is no longer required. The script purpose now
// decides the dispatch, not the redeemer. For backwards compatibility with the prior version's ABI,
// the prior redeemer can continue to be used; it is just ignored by the smart contract.
