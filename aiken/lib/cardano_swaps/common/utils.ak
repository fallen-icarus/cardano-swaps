use aiken/primitive/bytearray
use aiken/collection/list
use cardano/address.{
  Address,
  Inline,
  Script,
  VerificationKey,
  Credential,
  Pointer}
use aiken/crypto.{
  VerificationKeyHash,
  }
use cardano/assets.{PolicyId,Value,Lovelace}
use cardano_swaps/common/types.{AssetConfig}
use aiken/collection/pairs

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
  withdrawals: Pairs<Credential, Lovelace>,
  extra_signatories: List<VerificationKeyHash>
) -> Bool {
  let Address{stake_credential: staking_cred, ..} = swap_addr
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    Some(Inline(cred)) -> {
      when cred is {
        // If the address uses a staking pubkey, it must have signed the tx.
        VerificationKey(skey) -> list.has(extra_signatories,skey)
        // If the address uses a staking script, it must have been executed in the tx. In order for
        // the staking credential to show up in this dictionary, it must be registered.
        Script(_) -> pairs.has_key(withdrawals, cred)
      }
    }

    Some(Pointer {..}) -> False
  }
}

pub fn compare_asset_config(asset1: AssetConfig, asset2: AssetConfig) -> Ordering {
  let AssetConfig{asset_id: asset1_id,asset_name: asset1_name} = asset1
  let AssetConfig{asset_id: asset2_id,asset_name: asset2_name} = asset2
  when bytearray.compare(asset1_id,asset2_id) is {
    Less -> Less
    Greater -> Greater
    Equal -> bytearray.compare(asset1_name,asset2_name)
  }
}

pub fn has_beacon_script_minting_execution(
  beacon_id: PolicyId,
  mint: Value
) -> Bool {
  assets.to_pairs(mint) |> pairs.has_key(beacon_id)
}

pub fn has_beacon_script_staking_execution(
  beacon_id: PolicyId,
  withdrawals: Pairs<Credential, Int>,
) -> Bool {
  pairs.has_key(
    withdrawals,
    Script(beacon_id)
  )
}
