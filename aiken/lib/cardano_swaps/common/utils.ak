use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/transaction/credential.{
  Address,
  Inline,
  StakeCredential,
  VerificationKey,
  VerificationKeyCredential
  }
use aiken/transaction/value.{PolicyId,AssetName,MintedValue}

use cardano_swaps/common/types.{AssetConfig}

pub fn trace_if_false(msg: String, predicate: Bool) {
  if !predicate {
    trace msg
    predicate
  } else {
    predicate
  }
}

pub fn error_if_false(msg: String, predicate: Bool) {
  if !predicate {
    error msg
  } else {
    predicate
  }
}

// Check that either the staking pubkey signed or the staking script was executed.
pub fn staking_credential_approves(
  swap_addr: Address, 
  withdrawals: Dict<StakeCredential, Int>,
  extra_signatories: List<Hash<Blake2b_224, VerificationKey>>
) -> Bool {
  let Address(_,staking_cred) = swap_addr
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> {
      list.has(extra_signatories,skey)
    }

    // If the address uses a staking script, it must have been executed in the tx. In order for
    // the staking credential to show up in this dictionary, it must be registered.
    Some(svh) -> {
      dict.has_key(withdrawals,svh)
    }
  }
}

pub fn compare_asset_config(asset1: AssetConfig, asset2: AssetConfig) -> Ordering {
  let AssetConfig(asset1_id,asset1_name) = asset1
  let AssetConfig(asset2_id,asset2_name) = asset2
  when bytearray.compare(asset1_id,asset2_id) is {
    Less -> Less
    Greater -> Greater
    Equal -> bytearray.compare(asset1_name,asset2_name)
  }
}

// Create the asset name for the pair beacon by hashing the combination of asset1 and asset2. 
// It uses the sha2_256 hash algorithm. It assumes the pair is already sorted when used for 
// two-way swaps and assumes the first asset is the offer asset when used for one-way swaps. 
// When ADA is part of the pair, the empty bytestring is replaced so that the beacon is distinct 
// from the offer beacon.
pub fn generate_pair_beacon(asset1: AssetConfig, asset2: AssetConfig) -> AssetName {
  let AssetConfig(asset1_id_,asset1_name) = asset1
  let AssetConfig(asset2_id_,asset2_name) = asset2

  let asset1_id = if asset1_id_ == #"" { #"00" } else { asset1_id_ }
  let asset2_id = if asset2_id_ == #"" { #"00" } else { asset2_id_ }

  hash.sha2_256(
    bytearray.concat(asset1_id,asset1_name)
    |> bytearray.concat(_,asset2_id)
    |> bytearray.concat(_,asset2_name)
  )
}

// Create the asset name for the beacon by hashing the combination of the policy id and asset name
// for the asset. It uses the sha2_256 hash algorithm.
pub fn generate_offer_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  hash.sha2_256(
    bytearray.concat( policy_id, asset_name )
  )
}

pub fn beacons_only_burned(
  beacon_id: PolicyId,
  mint: MintedValue
) -> Bool {
  // Get all the beacons minted/burned in this transaction.
  let beacons_minted = 
    value.from_minted_value(mint)
    |> value.tokens(_,beacon_id)

  dict.foldl( beacons_minted, True, fn(_,v,r) {r && v < 0})
}

