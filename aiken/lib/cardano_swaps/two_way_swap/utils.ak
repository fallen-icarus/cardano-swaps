use aiken/cbor
use aiken/collection/dict.{Dict}
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/collection/list
use aiken/option.{is_some, is_none}
use aiken/primitive/string.{concat}
use cardano/transaction.{ValidityRange,Datum,InlineDatum,Output,OutputReference} as tx
use cardano/address.{
  Address,
  Script,
  }
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId,AssetName,Value}

use cardano_swaps/common/types.{app_name,AssetConfig,Rational} as common_types
use cardano_swaps/common/utils.{
  compare_asset_config,
  error_if_false,
  trace_if_false,
  } as common_utils
use cardano_swaps/two_way_swap/types.{
  SwapDatum,
  SwapAsset,
  Asset,
  SortedPair
  } as two_way_types

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

// Create the asset name for the pair beacon by hashing the serialised SortedPair. It uses the
// sha2_256 hash algorithm on the Data encoding, which is injective (see SwapAsset). It crashes
// if the pair is not sorted: asset1 < asset2.
pub fn generate_pair_beacon(pair: SwapAsset) -> AssetName {
  expect SortedPair(asset1_id, asset1_name, asset2_id, asset2_name) = pair
  expect True = error_if_false(
    @"Asset1 must be less than asset2",
    compare_asset_config(
      AssetConfig(asset1_id, asset1_name),
      AssetConfig(asset2_id, asset2_name)
    ) == Less
  )
  crypto.sha2_256(cbor.serialise(pair))
}

// Create the asset name for an asset beacon by hashing the serialised Asset. It uses the
// sha2_256 hash algorithm on the Data encoding, which is injective (see SwapAsset).
pub fn generate_asset_beacon(asset: SwapAsset) -> AssetName {
  expect Asset(_,_) = asset
  crypto.sha2_256(cbor.serialise(asset))
}

// This is used to check that the value of Swap UTxOs are correct. It also returns the amount of
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
  let foo = fn(x: Pair<PolicyId,Dict<AssetName,Int>>, acc: (Int,Int,Int,Bool)) {
    let Pair(cn,tns_dict) = x
    let (offer,ask,ada,valid_beacons) = acc

    // Check the beacons.
    if cn == beacon_id {
      let expected_dict =
          dict.from_pairs([
            Pair(pair_beacon, 1),
            Pair(offer_beacon, 1),
            Pair(ask_beacon, 1),
          ])

      // UTxO must have the right beacons.
      expect True = error_if_false(
          @"UTxO has wrong beacons",
          tns_dict == expected_dict
      )

      (offer, ask, ada, True)
    } 

    // Check ada.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      // Also account for when ada is part of the pair.
      if cn == offer_id {
        (offer+n,ask,ada+n,valid_beacons)
      } else if cn == ask_id {
        (offer,ask+n,ada+n,valid_beacons)
      } else {
        (offer,ask,ada+n,valid_beacons)
      }
    }

    // Must handle when the offer and ask assets have the same policy id.
    else if cn == offer_id && cn == ask_id {
      when dict.to_pairs(tns_dict) is {
        // Only one of the assets is present.
        [Pair(tn,n)] -> {
          if tn == offer_name {
            (offer+n,ask,ada,valid_beacons)
          } else if tn == ask_name {
            (offer,ask+n,ada,valid_beacons)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }

        // They are both present in the UTxO.
        [Pair(tn1,n1),Pair(tn2,n2)] -> {
          if tn1 == offer_name && tn2 == ask_name {
              (offer+n1,ask+n2,ada,valid_beacons)
          } else if tn1 == ask_name && tn2 == offer_name {
              (offer+n2,ask+n1,ada,valid_beacons)
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
            (offer+n,ask,ada,valid_beacons)
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
            (offer,ask+n,ada,valid_beacons)
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

  let (offer,ask,ada,valid_beacons) = list.foldl(
    assets.to_dict(val) |> dict.to_pairs(_),
    (0,0,0,False),
    foo
  )

  if valid_beacons {
    (offer,ask,ada)
  } else {
    error @"UTxO has wrong beacons"
  }
}

// This checks the datum and calls extract_ask_and_offer_quantity to check the value.
fn valid_swap_output(
  beacon_id: PolicyId,
  output_value: Value,
  output_datum: Datum,
  invalid_hereafter: Option<Int>,
) -> Bool {
  let SwapDatum{
    beacon_id: beacon_id_,
    pair_beacon: pair_beacon_,
    asset1_id: asset1_id_,
    asset1_name: asset1_name_,
    asset1_beacon: asset1_beacon_,
    asset2_id: asset2_id_,
    asset2_name: asset2_name_,
    asset2_beacon: asset2_beacon_,
    asset1_price: asset1_price_,
    asset2_price: asset2_price_,
    expiration: expiration_,
    ..
  } = parse_datum(output_datum)

  let (o,_,_) =
    extract_ask_and_offer_quantity(
      output_value,
      beacon_id,
      pair_beacon_,
      asset1_id_,
      asset1_name_,
      asset1_beacon_,
      asset2_id_,
      asset2_name_,
      asset2_beacon_
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

  // The pair_beacon must be the sha2_256 hash of the serialised SortedPair. generate_pair_beacon
  // also enforces that asset1 < asset2.
  trace_if_false(
    @"Wrong pair_beacon",
    pair_beacon_ == generate_pair_beacon(SortedPair(asset1_id_,asset1_name_,asset2_id_,asset2_name_))
  ) &&

  // The asset1_beacon must be the sha2_256 hash of the serialised Asset.
  trace_if_false(
    @"Wrong asset1_beacon",
    asset1_beacon_ == generate_asset_beacon(Asset(asset1_id_,asset1_name_))
  ) &&

  // The asset2_beacon must be the sha2_256 hash of the serialised Asset.
  trace_if_false(
    @"Wrong asset2_beacon",
    asset2_beacon_ == generate_asset_beacon(Asset(asset2_id_,asset2_name_))
  ) &&

  // The asset1_price denominator must be > 0.
  trace_if_false( 
    @"asset1_price denominator not > 0", 
    asset1_price_.denominator > 0 
  ) &&

  // The asset1_price must be > 0.
  trace_if_false(
    @"asset1_price numerator not > 0", 
    asset1_price_.numerator > 0 
  ) &&

  // The asset2_price denominator must be > 0.
  trace_if_false( 
    @"asset2_price denominator not > 0", 
    asset2_price_.denominator > 0
  ) &&

  // The asset2_price must be > 0.
  trace_if_false( 
    @"asset2_price numerator not > 0", 
    asset2_price_.numerator > 0
  ) &&

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
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The beacons must go to an address protected by the DApp validator script.
  // 2) The beacons must go to an address using a valid staking credential.
  // 3) The beacons cannot be stored with a reference script.
  // 4) The UTxOs with the beacons must have the proper value:
  //      a) Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
  //      b) The beacons must correspond to the beacons in the datum.
  //      c) There must be exactly 1 of each beacon.
  //      e) No extraneous assets are in the UTxO.
  // 5) The beacons must be stored with the proper inline `SwapDatum`:
  //      a) `beacon_id` == this policy id.
  //      b) `pair_beacon` == sha2_256(serialise(SortedPair(asset1_id, asset1_name, asset2_id, asset2_name)))
  //      c) `asset1_id` == policy id of asset1 for that trading pair.
  //      d) `asset1_name` == asset name of asset1 for that trading_pair.
  //      e) `asset1_beacon` == sha2_256(serialise(Asset(asset1_id, asset1_name))).
  //      f) `asset2_id` == policy id of asset2 for that trading_pair.
  //      g) `asset2_name` == asset name of asset2 for that trading_pair.
  //      h) `asset2_beacon` == sha2_256(serialise(Asset(asset2_id, asset2_name))).
  //      i) `asset1_price` denominator > 0
  //      j) `asset1_price` numerator > 0
  //      k) `asset2_price` denominator > 0
  //      l) `asset2_price` numerator > 0
  //      m) asset1 < asset2 (enforced by generate_pair_beacon)
  //      n) `expiration` >= invalid-hereafter if expiration used
  // 6) The offer asset and the ask asset must be different assets. Asset1 must be lexicographically
  // less than Asset2.
  //
  // The validator assumes the trading pairs are sorted which is why asset1 must be less than
  // asset2. Asset1 and asset2 cannot be the same asset.

  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output{
      address: Address{payment_credential: pay, stake_credential: stake}, 
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
        // All previous outputs must be valid.
        acc &&

        // The output must be at a DApp address with staking. Uses `app_name` in 
        // the error message.
        trace_if_false( 
          concat(@"Beacon must go to a ", app_name) |> concat(_,@" DApp address with staking"),
          pay == Script(dapp_hash) && is_some(stake)
        ) &&

        // The output cannot hold a reference script.
        trace_if_false( 
          @"Swap cannot hold a reference script",
          is_none(ref_script)
        ) &&

        // The output must be properly configured. This has its own trace messages.
        valid_swap_output(beacon_id,val,d,invalid_hereafter)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Two-way swaps must have exactly three kinds of beacons"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl( outputs, True, foo )
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
  asset1_id: PolicyId,
  asset1_name: AssetName,
  asset1_beacon: AssetName,
  asset2_id: PolicyId,
  asset2_name: AssetName,
  asset2_beacon: AssetName,
  asset1_price: Rational,
  asset2_price: Rational,
  expiration: Option<Int>,
  outputs: List<Output>,
  taking_asset2: Bool // Whether asset2 is being taken.
) -> Bool {
  
  let req_datum =
    SwapDatum(
      beacon_id,
      pair_beacon,
      asset1_id,
      asset1_name,
      asset1_beacon,
      asset2_id,
      asset2_name,
      asset2_beacon,
      asset1_price,
      asset2_price,
      Some(input_ref),
      expiration
    ) |> InlineDatum

  // Make sure the swap is not already expired.
  expect True = when (expiration, invalid_hereafter) is {
    (None, _) -> True
    (Some(expir), Some(hereafter)) -> 
      error_if_false(@"invalid-hereafter must be <= expiration", hereafter <= expir)
    (Some(_),None) -> error "invalid-hereafter required but not set"
  }

  // Which asset is ask/offer and what price should be used.
  let (offer_id,offer_name,offer_beacon,ask_id,ask_name,ask_beacon,price) = 
    if taking_asset2 { 
      (asset2_id,asset2_name,asset2_beacon,asset1_id,asset1_name,asset1_beacon,asset2_price) 
    } else { 
      (asset1_id,asset1_name,asset1_beacon,asset2_id,asset2_name,asset2_beacon,asset1_price) 
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
    offer_taken * price.numerator <= ask_given * price.denominator
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
test golden_asset_beacon_ada() {
  generate_asset_beacon(Asset(#"", #"")) ==
    #"07d5f63e85046b83e1fc4102a7c19c3f1711c51984725e3b6cf195900947cebe"
}

// Pre-image: d8799f581c00112233445566778899aabbccddeeff00112233445566778899aabb44444a4544ff
test golden_asset_beacon_native_asset() {
  generate_asset_beacon(Asset(test_policy, test_name)) ==
    #"2dab55ec954afe698b6de75d81f0067fd503d1e4ecd49d593f8a3b9c9a963d4f"
}

// A 32-byte asset name crosses CBOR's 23-byte short-length boundary (0x5820 prefix).
// Pre-image: d8799f581c...aabb5820ffeeddcc...1100ff
test golden_asset_beacon_max_length_name() {
  generate_asset_beacon(Asset(test_policy, test_max_name)) ==
    #"130f1a4999dcc8b6bb9f31949ee44381e587ace2f03437c65d15536770704324"
}

// Ada sorts first, so it is always asset1 when part of a pair.
// Pre-image: d87a9f4040581c00112233445566778899aabbccddeeff00112233445566778899aabb44444a4544ff
test golden_pair_beacon_ada_and_token() {
  generate_pair_beacon(SortedPair(#"", #"", test_policy, test_name)) ==
    #"c6656f7142e3ef8e72066251cac9e9cc2454571204fa85ab13b34a03e77d7230"
}

// The old concatenation scheme collided here: the asset beacon pre-hash "123" ++ "ABC" and the
// pair beacon pre-hash "12" ++ "3" ++ "AB" ++ "C" both concatenate to "123ABC". The constructor
// tags and field framing keep the two beacon kinds disjoint.
// Pre-images: d8799f4331323343414243ff vs d87a9f42313241334241424143ff
test asset_and_pair_beacons_cannot_collide() {
  generate_asset_beacon(Asset("123", "ABC")) ==
    #"421118aa9387559826262aa7545bfac75f5393f4867a8bb5ab0df111e96aeb15" &&
  generate_pair_beacon(SortedPair("12", "3", "AB", "C")) ==
    #"0980838c69e2b4193015eed4505d222849ecca60fdf069f75a57f250c1762d61"
}

// The old concatenation scheme collided here: "AB" ++ "" ++ "CD" ++ "" and
// "AB" ++ "C" ++ "D" ++ "" both concatenate to "ABCD". The Data encoding length-prefixes every
// field, so the boundaries cannot shift.
// Pre-images: d87a9f4241424042434440ff vs d87a9f4241424143414440ff
test pair_beacon_field_boundaries_cannot_shift() {
  generate_pair_beacon(SortedPair("AB", "", "CD", "")) ==
    #"6e90ae87cd390983bb293f04b40d22c04fe00c0957eca07f68d63e63d54a005e" &&
  generate_pair_beacon(SortedPair("AB", "C", "D", "")) ==
    #"97bd089a14e634317d6251349086ede4eaff43878495246e925b78bfb8534224"
}

// The generate_ functions must crash when given the wrong SwapAsset variant.
test asset_beacon_rejects_pair() fail {
  generate_asset_beacon(SortedPair(#"", #"", test_policy, test_name)) == #""
}

test pair_beacon_rejects_single_asset() fail {
  generate_pair_beacon(Asset(test_policy, test_name)) == #""
}

// generate_pair_beacon must crash on unsorted or equal pairs.
test pair_beacon_rejects_unsorted_pair() fail {
  generate_pair_beacon(SortedPair(test_policy, test_name, #"", #"")) == #""
}

test pair_beacon_rejects_equal_assets() fail {
  generate_pair_beacon(SortedPair(test_policy, test_name, test_policy, test_name)) == #""
}
