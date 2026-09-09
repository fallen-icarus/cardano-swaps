use aiken/cbor
use aiken/collection/dict.{Dict}
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/collection/list
use aiken/option.{is_some, is_none}
use aiken/primitive/string.{concat}
use cardano/transaction.{ValidityRange,Datum,InlineDatum,Output,OutputReference} as tx
use cardano/address.{
  Address,
  Credential,
  Script,
  StakeCredential,
  }
use aiken/crypto.{ScriptHash,VerificationKeyHash}
use cardano/assets.{PolicyId,Value,AssetName,Lovelace}

use cardano_swaps/common/types.{app_name,AssetConfig,Rational} as common_types
use cardano_swaps/common/utils.{
  error_if_false,
  trace_if_false,
  staking_credential_approves
  } as common_utils
use cardano_swaps/one_way_swap/types.{
  SwapDatum,
  OfferAsset,
  AskAsset,
  TradingPair
  } as one_way_types

// This parses the datums attached to target UTxOs. It will fail with an error unless
// the datum is an inline SwapDatum.
fn parse_datum(raw_datum: Datum) -> SwapDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect swap_datum: SwapDatum = d
      swap_datum
    }
    _ -> error @"All swap datums must be inline datums"
  }
}

// Get the time from the tx's invalid-hereafter setting. The invalid-hereafter setting is used for
// expiration times since it guarantees that the specified time has NOT passed yet; the transaction
// would fail otherwise. By having users set the invalid-hereafter to the closest expiration time,
// users can assert to the plutus script that the expiration indeed has not passed. This returns an
// Option in case the expiration is not actually used.
pub fn get_upper_bound(val_range: ValidityRange) -> Option<Int> {
  when val_range is {
    Interval{upper_bound: IntervalBound{bound_type: Finite(x), ..}, ..} -> Some(x)
    _ -> None
  }
}

// Create the asset name for the pair beacon by hashing the serialised TradingPair. It uses the
// sha2_256 hash algorithm on the Data encoding, which is injective (see SwapAsset). The offer
// asset must come first.
pub fn generate_pair_beacon(
  offer_id: PolicyId,
  offer_name: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName
) -> AssetName {
  crypto.sha2_256(cbor.serialise(TradingPair(offer_id,offer_name,ask_id,ask_name)))
}

// Create the asset name for the offer beacon by hashing the serialised OfferAsset. It uses the
// sha2_256 hash algorithm on the Data encoding, which is injective (see SwapAsset).
pub fn generate_offer_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  crypto.sha2_256(cbor.serialise(OfferAsset(policy_id,asset_name)))
}

// Create the asset name for the ask beacon by hashing the serialised AskAsset. It uses the
// sha2_256 hash algorithm on the Data encoding, which is injective (see SwapAsset).
pub fn generate_ask_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  crypto.sha2_256(cbor.serialise(AskAsset(policy_id,asset_name)))
}

// This is used to check thath the value of Swap UTxOs are correct. It also returns the amount of
// the offer asset, ask asset, and ada so that it can be used during swap execution as well. It will
// crash the script if any Swap UTxOs are malformed.
fn extract_ask_and_offer_quantity(
  val: Value,
  beacon_id: PolicyId,
  pair_beacon: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  offer_beacon: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName,
  ask_beacon: AssetName
) -> (Int,Int,Int) {
  // Separate the beacon tokens from the rest of the value to check them separately.
  let (beacon_tns, rest) = assets.to_dict(val) |> dict.pop(_, beacon_id)

  let expected_dict =
      dict.from_pairs([
        Pair(pair_beacon, 1),
        Pair(offer_beacon, 1),
        Pair(ask_beacon, 1),
      ])

  // UTxO must have the right beacons.
  expect True = error_if_false(
      @"UTxO has wrong beacons",
      beacon_tns == Some(expected_dict)
  )

  let foo = fn(x: Pair<PolicyId,Dict<AssetName,Int>>, acc: (Int,Int,Int)) {
    let Pair(cn,tns_dict) = x
    let (offer,ask,ada) = acc

    // Check ada.
    if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      // Also account for when ada is part of the pair.
      if cn == offer_id && offer_name == #"" {
        (offer+n,ask,ada+n)
      } else if cn == ask_id && ask_name == #"" {
        (offer,ask+n,ada+n)
      } else {
        (offer,ask,ada+n)
      }
    }

    // Must handle when the offer and ask assets have the same policy id.
    else if cn == offer_id && cn == ask_id {
      when dict.to_pairs(tns_dict) is {
        // Only one of the assets is present.
        [Pair(tn,n)] -> {
          if tn == offer_name {
            (offer+n,ask,ada)
          } else if tn == ask_name {
            (offer,ask+n,ada)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }

        // They are both present in the UTxO.
        [Pair(tn1,n1),Pair(tn2,n2)] -> {
          if tn1 == offer_name && tn2 == ask_name {
              (offer+n1,ask+n2,ada)
          } else if tn1 == ask_name && tn2 == offer_name {
              (offer+n2,ask+n1,ada)
          } else {
              error @"No extraneous assets allowed in the UTxO"
          }
        }

        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    }

    // Check the offer asset. If this path is taken, the ask asset and offer asset have different
    // policy ids.
    else if cn == offer_id {
      when dict.to_pairs(tns_dict) is {
        [Pair(tn,n)] -> {
          if tn == offer_name {
            (offer+n,ask,ada)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }
        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    }

    // Check the ask asset. If this path is taken, the ask asset and offer asset have different
    // policy ids.
    else if cn == ask_id {
      when dict.to_pairs(tns_dict) is {
        [Pair(tn,n)] -> {
          if tn == ask_name {
            (offer,ask+n,ada)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }
        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    } else {
      error @"No extraneous assets allowed in the UTxO"
    }
  }

  list.foldl( dict.to_pairs(rest), (0,0,0), foo )
}

// This checks the datum and calls extract_ask_and_offer_quantity to check the value.
fn valid_swap_output(
  beacon_id: PolicyId,
  output_value: Value,
  swap_datum: SwapDatum,
  invalid_hereafter: Option<Int>,
) -> Bool {
  let SwapDatum{
    beacon_id: beacon_id_,
    pair_beacon: pair_beacon_,
    offer_id: offer_id_,
    offer_name: offer_name_,
    offer_beacon: offer_beacon_,
    ask_id: ask_id_,
    ask_name: ask_name_,
    ask_beacon: ask_beacon_,
    swap_price: swap_price_,
    expiration: expiration_,
    ..
  } = swap_datum

  let offer = AssetConfig(offer_id_,offer_name_)
  let ask = AssetConfig(ask_id_,ask_name_)

  let (o,_,_) = 
    extract_ask_and_offer_quantity(
      output_value,
      beacon_id,
      pair_beacon_,
      offer_id_,
      offer_name_,
      offer_beacon_,
      ask_id_,
      ask_name_,
      ask_beacon_
    )

  // The order must not be expired. If the order does not have a set expiration, this check
  // can be skipped. There is no need for invalid-hereafter if the order does not expire. If an
  // expiration is set, it must fall on a 1-min interval. invalid-hereafter is used to prove to the
  // smart contract that the expiration time has not passed.
  expect True = when (expiration_,invalid_hereafter) is {
    (None,_) -> True
    (Some(expir),Some(hereafter)) -> 
      error_if_false(@"Expiration must be >= invalid-hereafter", hereafter <= expir) &&
      error_if_false(@"Must use 1-min expiration intervals", expir % 60000 == 0)
    (Some(_),None) -> error "invalid-hereafter required but not set"
  }

  // The beacon_id must be the policy id of the beacon policy.
  trace_if_false( @"Wrong beacon_id", beacon_id == beacon_id_ ) &&

  // The offer asset must be a different asset than the ask asset.
  trace_if_false( @"Offer asset cannot be same as ask asset", offer != ask ) &&

  // The pair_beacon must be the sha2_256 hash of the serialised TradingPair.
  trace_if_false(
    @"Wrong pair_beacon",
    pair_beacon_ == generate_pair_beacon(offer_id_,offer_name_,ask_id_,ask_name_)
  ) &&

  // The offer_beacon must be the sha2_256 hash of the serialised OfferAsset.
  trace_if_false(
    @"Wrong offer_beacon",
    offer_beacon_ == generate_offer_beacon(offer_id_,offer_name_)
  ) &&

  // The ask_beacon must be the sha2_256 hash of the serialised AskAsset.
  trace_if_false(
    @"Wrong ask_beacon",
    ask_beacon_ == generate_ask_beacon(ask_id_,ask_name_)
  ) &&

  // The swap_price denominator must be > 0.
  trace_if_false( 
    @"swap_price denominator not > 0", 
    swap_price_.denominator > 0 
  ) &&

  // The swap_price must be > 0.
  trace_if_false(
    @"swap_price numerator not > 0", 
    swap_price_.numerator > 0 
  ) &&

  // If the offer asset is ADA, it must be properly named.
  trace_if_false( @"Invalid asset", if offer_id_ == #"" { offer_name_ == #"" } else { True }) &&

  // If the ask asset is ADA, it must be properly named.
  trace_if_false( @"Invalid asset", if ask_id_ == #"" { ask_name_ == #"" } else { True }) &&

  // This is just to force the extract_ask_and_offer_quantity function to be executed.
  o >= 0
}

// Check if the beacons are locked at the proper address and the UTxO is configured properly.
// This function will return `True` or `False`. It uses `app_name` in the error message to generate
// a unique beacon policy for testing. 
pub fn beacon_destination_check(
  dapp_hash: ScriptHash,
  beacon_id: PolicyId,
  invalid_hereafter: Option<Int>,
  withdrawals: Pairs<Credential, Lovelace>,
  extra_signatories: List<VerificationKeyHash>,
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The beacons must go to an address protected by the DApp validator script.
  // 2) The beacons must go to an address using a valid staking credential. If the swap is new
  //    (`prev_input` == None), the staking credential must approve the transaction. This proves the
  //    credential is usable so that funds cannot be accidentally locked at an unusable address.
  //    Outputs with `prev_input` == Some(_) are skipped so swap executions remain composable. The
  //    approval is only re-checked when a new swap for a different staking credential is found.
  // 3) The beacons cannot be stored with a reference script.
  // 4) The UTxOs with the beacons must have the proper value:
  //      a) Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
  //      b) The beacons must correspond to the beacons in the datum.
  //      c) There must be exactly 1 of each beacon.
  //      d) No extraneous assets are in the UTxO.
  // 5) The beacons must be stored with the proper inline `SwapDatum`:
  //      a) `beacon_id` == this policy id.
  //      b) `pair_beacon` == `sha2_256(serialise(TradingPair(offer_id, offer_name, ask_id, ask_name)))`
  //      c) `offer_id` == policy id of the offer asset.
  //      d) `offer_name` == asset name of the offer asset.
  //      e) `offer_beacon` == `sha2_256(serialise(OfferAsset(offer_id, offer_name)))`.
  //      f) `ask_id` == policy id of the ask asset.
  //      g) `ask_name` == asset name of the ask asset.
  //      h) `ask_beacon` == `sha2_256(serialise(AskAsset(ask_id, ask_name)))`
  //      i) `swap_price` denominator > 0
  //      j) `swap_price` numerator > 0
  //      k) `expiration` >= invalid-hereafter if expiration used
  // 6) The offer asset and the ask asset must be different assets.

  // A function to map over the list of outputs. The accumulator also tracks the staking credential
  // that most recently approved a new swap so it is not re-checked for the next new swap.
  let foo = fn (out: Output, acc: (Bool, Option<StakeCredential>)) {
    let (valid, last_approved) = acc

    // Extract out the output's information.
    let Output{
      address: Address{payment_credential: pay,stake_credential: stake},
      value: val,
      datum: d, 
      reference_script: ref_script,
    } = out

    // Get the beacons in this UTxO and save it as a list for pattern matching.
    let beacon_list = assets.tokens(val,beacon_id) |> dict.to_pairs()

    when beacon_list is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has three types of beacon.
      [Pair(_,_),Pair(_,_),Pair(_,_)] -> {
        let swap_datum = parse_datum(d)

        // New swaps (prev_input == None) must have their staking credential approve.
        let needs_approval = swap_datum.prev_input == None && stake != last_approved

        ( // All previous outputs must be valid.
          valid &&

          // The output must be at a DApp address with staking. Uses `app_name` in
          // the error message.
          trace_if_false(
            concat(@"Beacon must go to a ", app_name) |> concat(_,@" DApp address with staking"),
            pay == Script(dapp_hash) && is_some(stake)
          ) &&

          // The staking credential must approve new swaps.
          trace_if_false(
            @"Staking credential did not approve",
            !needs_approval || staking_credential_approves(out.address, withdrawals, extra_signatories)
          ) &&

          // The output cannot hold a reference script.
          trace_if_false(
            @"Swap cannot hold a reference script",
            is_none(ref_script)
          ) &&

          // The output must be properly configured. This has its own trace messages.
          valid_swap_output(beacon_id,val,swap_datum,invalid_hereafter)
        , if needs_approval { stake } else { last_approved }
        )
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"One-way swaps must have exactly three kinds of beacons"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl( outputs, (True, None), foo ).1st
}

// This is a recursive function that will terminate when the corresponding output is found or when
// no corresponding output is found. When no corresponding output is found, the function will throw
// an error. It looks for the required datum first to minimize the number of checks needed.
fn swap_output_value(
  swap_addr: Address,
  beacon_id: PolicyId,
  pair_beacon: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  offer_beacon: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName,
  ask_beacon: AssetName,
  req_datum: Datum,
  outputs: List<Output>
) -> (Int,Int,Int) {
  when outputs is {
    [] -> error @"Corresponding swap output not found"
    [output, ..rest] -> {
      // Requirements for a valid corresponding output:
      // 1) The output is to the same swap address where the input originates.
      // 2) The output has exactly one of each of the proper beacons.
      // 3) The output's datum is exactly the same accept the `prev_input` == `Some(input_ref)`.

      // The swap output must have:
      // 1. The correct datum.
      // 2. Be locked at the proper address.
      // 3. Not contain a reference script. While not an attack since the person who places the
      // reference script effectively gives the next executor the ADA for the deposit, it can cause
      // the execution usage to spike which can impact composability.
      if swap_addr == output.address && req_datum == output.datum && is_none(output.reference_script) {
          // Get the necessary values. It will also check if the UTxO is properly configured.
          extract_ask_and_offer_quantity(
            output.value,
            beacon_id,
            pair_beacon,
            offer_id,
            offer_name,
            offer_beacon,
            ask_id,
            ask_name,
            ask_beacon
          )
      }

      // This cannot be the corresponding output so recurse.
      else {
        swap_output_value(
          swap_addr,
          beacon_id,
          pair_beacon,
          offer_id,
          offer_name,
          offer_beacon,
          ask_id,
          ask_name,
          ask_beacon,
          req_datum,
          rest
        )
      }
    }
  }
}

// Compare the input and output to check that the swap is valid.
pub fn valid_swap(
  input_ref: OutputReference,
  input_val: Value,
  invalid_hereafter: Option<Int>,
  swap_addr: Address,
  beacon_id: PolicyId,
  pair_beacon: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  offer_beacon: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName,
  ask_beacon: AssetName,
  swap_price: Rational,
  expiration: Option<Int>,
  outputs: List<Output>,
) -> Bool {
  let req_datum = 
    SwapDatum(
      beacon_id,
      pair_beacon,
      offer_id,
      offer_name,
      offer_beacon,
      ask_id,
      ask_name,
      ask_beacon,
      swap_price,
      Some(input_ref),
      expiration,
    ) |> InlineDatum

  // Make sure the swap is not already expired.
  expect True = when (expiration, invalid_hereafter) is {
    (None, _) -> True
    (Some(expir), Some(hereafter)) -> 
      error_if_false(@"invalid-hereafter must be <= expiration", hereafter <= expir)
    (Some(_),None) -> error "invalid-hereafter required but not set"
  }

  // Get the values in the inputs.
  let (offer_i,ask_i,ada_i) =
    extract_ask_and_offer_quantity(
      input_val,
      beacon_id,
      pair_beacon,
      offer_id,
      offer_name,
      offer_beacon,
      ask_id,
      ask_name,
      ask_beacon
    )

  // Get the values in the outputs.
  let (offer_o,ask_o,ada_o) =
    swap_output_value(
      swap_addr,
      beacon_id,
      pair_beacon,
      offer_id,
      offer_name,
      offer_beacon,
      ask_id,
      ask_name,
      ask_beacon,
      req_datum,
      outputs
    )

  // Calculate the quantity of the ask asset in flux. Output - Input.
  let ask_given = ask_o - ask_i

  // Calculate the quantity of the offer asset in flux. Input - Output.
  let offer_taken = offer_i - offer_o

  // Ada can always be deposited if not part of the pair.
  expect True = 
    if offer_id == #"" || ask_id == #"" {
      True
    } else {
      error_if_false(
        @"Ada can only be deposited",
        ada_o - ada_i >= 0
      )
    }

  // The swap_ratio must be correct. To avoid rounding issues, the ask_given is multiplied
  // by the price_den. This is mathematically equivalent to:
  // offer_taken * price_num / price_den <= ask_given
  trace_if_false( 
    @"Fail: offer_taken * price <= ask_given",
    offer_taken * swap_price.numerator <= ask_given * swap_price.denominator
  ) &&

  // The ask asset cannot be taken. This would allow the swap to go in reverse.
  trace_if_false(
    @"The ask asset cannot be taken from the swap",
    ask_given >= 0 // Negative would mean it was taken.
  )
}

// =================================================================================================
// Golden tests for beacon name derivation.
// =================================================================================================

const test_policy: PolicyId = #"00112233445566778899aabbccddeeff00112233445566778899aabb"

const test_name: AssetName = #"444a4544"

// A 32-byte asset name (the maximum length).
const test_max_name: AssetName =
  #"ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100"

// Pre-image: d8799f4040ff
test golden_offer_beacon_ada() {
  generate_offer_beacon(#"", #"") ==
    #"07d5f63e85046b83e1fc4102a7c19c3f1711c51984725e3b6cf195900947cebe"
}

// Pre-image: d87a9f4040ff
test golden_ask_beacon_ada() {
  generate_ask_beacon(#"", #"") ==
    #"08bae3e35a3531a500149bd10d9b872621a41b4f6ba086920518220829370d2b"
}

// Pre-image: d8799f581c00112233445566778899aabbccddeeff00112233445566778899aabb44444a4544ff
test golden_offer_beacon_native_asset() {
  generate_offer_beacon(test_policy, test_name) ==
    #"2dab55ec954afe698b6de75d81f0067fd503d1e4ecd49d593f8a3b9c9a963d4f"
}

// Pre-image: d87a9f581c00112233445566778899aabbccddeeff00112233445566778899aabb44444a4544ff
test golden_ask_beacon_native_asset() {
  generate_ask_beacon(test_policy, test_name) ==
    #"8530f818d752ab63a9366d8e5e38f1e83d45ae71240b08059fd8a3d4a2b6fea7"
}

// A 32-byte asset name crosses CBOR's 23-byte short-length boundary (0x5820 prefix).
// Pre-image: d8799f581c...aabb5820ffeeddcc...1100ff
test golden_offer_beacon_max_length_name() {
  generate_offer_beacon(test_policy, test_max_name) ==
    #"130f1a4999dcc8b6bb9f31949ee44381e587ace2f03437c65d15536770704324"
}

// Pre-image: d87b9f4040581c00112233445566778899aabbccddeeff00112233445566778899aabb44444a4544ff
test golden_pair_beacon_ada_to_token() {
  generate_pair_beacon(#"", #"", test_policy, test_name) ==
    #"ac77181ee78354aa42afae2ad4b902e3c6f9137525d3c56fe20ea2ffc0a6a878"
}

// Pre-image: d87b9f581c00112233445566778899aabbccddeeff00112233445566778899aabb44444a45444040ff
test golden_pair_beacon_token_to_ada() {
  generate_pair_beacon(test_policy, test_name, #"", #"") ==
    #"ccf1b9d64f82b9d3b5f6bf4b0ac4ab0a55e489c20f7c078a4621c6ab2b83b717"
}

// The old concatenation scheme required substituting "00" for ada's empty policy id to keep the
// two directions distinct. The Data encoding preserves direction on its own.
test pair_beacon_is_directional_for_ada() {
  generate_pair_beacon(#"", #"", test_policy, test_name) !=
    generate_pair_beacon(test_policy, test_name, #"", #"")
}

// The old concatenation scheme collided here: "ABC" ++ "" ++ "123" ++ "" and
// "ABC" ++ "123" ++ "" ++ "" both concatenate to "ABC123". The Data encoding length-prefixes
// every field, so the boundaries cannot shift.
// Pre-images: d87b9f43414243404331323340ff vs d87b9f43414243433132334040ff
test pair_beacon_field_boundaries_cannot_shift() {
  generate_pair_beacon("ABC", "", "123", "") ==
    #"461fd2d8a7179e276cde2b803a73978fb7a3e74000b1efd3cf3fdb89b1424dd6" &&
  generate_pair_beacon("ABC", "123", "", "") ==
    #"b9f1919a23126a9d272901f6cfb3a44c27ac683da940966293db88406f79d378"
}

// The constructor tags keep the offer and ask beacons distinct for the same asset.
test offer_and_ask_beacons_are_distinct() {
  generate_offer_beacon(test_policy, test_name) !=
    generate_ask_beacon(test_policy, test_name)
}
