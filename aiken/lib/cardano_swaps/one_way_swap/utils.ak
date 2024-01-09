use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/option.{is_some}
use aiken/string.{concat}
use aiken/transaction.{Datum,InlineDatum,Output,OutputReference} as tx
use aiken/transaction/credential.{
  Address,
  Script,
  ScriptCredential,
  }
use aiken/transaction/value.{PolicyId,AssetName,Value}

use cardano_swaps/common/types.{app_name,AssetConfig,Rational} as common_types
use cardano_swaps/common/utils.{
  error_if_false,
  trace_if_false
  } as common_utils
use cardano_swaps/one_way_swap/types.{
  SwapDatum
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

// Create the asset name for the pair beacon by hashing the combination of asset1 and asset2. 
// It uses the sha2_256 hash algorithm. It assumes the first asset is the offer asset.
// When ada is part of the pair, the empty bytestring is replaced so that the beacon is distinct 
// for each direction when ada is part of the pair.
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
// for the asset. It uses the sha2_256 hash algorithm. The pre-hash is prefixed with "01" to keep
// it distinct from the other beacons.
pub fn generate_offer_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  hash.sha2_256(
    bytearray.concat( 
      #"01",
      bytearray.concat( policy_id, asset_name )
    )
  )
}

// Create the asset name for the beacon by hashing the combination of the policy id and asset name
// for the asset. It uses the sha2_256 hash algorithm. The pre-hash is prefixed with "02" to keep
// it distinct from the other beacons.
pub fn generate_ask_beacon(policy_id: PolicyId, asset_name: AssetName) -> AssetName {
  hash.sha2_256(
    bytearray.concat( 
      #"02",
      bytearray.concat( policy_id, asset_name )
    )
  )
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
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Int,Int,Bool)) {
    let (cn,tns_dict) = x
    let (offer,ask,ada,valid_beacons) = acc

    // Check the beacons.
    if cn == beacon_id {
      let tns_list = dict.to_list(tns_dict)

      let beacon_check = fn(y: (AssetName,Int), acc: (Bool,Bool,Bool)) {
        let (tn,n) = y
        let (pair_check,offer_check,ask_check) = acc
        if tn == pair_beacon {
          (n == 1, offer_check, ask_check)
        } else if tn == offer_beacon {
          (pair_check, n == 1, ask_check)
        } else if tn == ask_beacon {
          (pair_check, offer_check, n == 1)
        } else {
          error @"UTxO has wrong beacons"
        }
      }

      when list.foldl(tns_list,(False,False,False),beacon_check) is {
        (True,True,True) -> (offer,ask,ada,True)
        _ -> acc
      }
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
      when dict.to_list(tns_dict) is {
        // Only one of the assets is present.
        [(tn,n)] -> {
          if tn == offer_name {
            (offer+n,ask,ada,valid_beacons)
          } else if tn == ask_name {
            (offer,ask+n,ada,valid_beacons)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }

        // They are both present in the UTxO.
        [(tn1,n1),(tn2,n2)] -> {
          if tn1 == offer_name {
            if tn2 == ask_name {
              (offer+n1,ask+n2,ada,valid_beacons)
            } else {
              error @"No extraneous assets allowed in the UTxO"
            }
          } else if tn1 == ask_name { 
            if tn2 == offer_name {
              (offer+n2,ask+n1,ada,valid_beacons)
            } else {
              error @"No extraneous assets allowed in the UTxO"
            }
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
      when dict.to_list(tns_dict) is {
        [(tn,n)] -> {
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
      when dict.to_list(tns_dict) is {
        [(tn,n)] -> {
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
    value.to_dict(val) |> dict.to_list(_),
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
  output_datum: Datum
) -> Bool {
  let SwapDatum(
    beacon_id_,
    pair_beacon_,
    offer_id_,
    offer_name_,
    offer_beacon_,
    ask_id_,
    ask_name_,
    ask_beacon_,
    swap_price_,
    _ // prev_input does not need to be check.
  ) = parse_datum(output_datum)

  let offer = AssetConfig(offer_id_,offer_name_)
  let ask = AssetConfig(ask_id_,ask_name_)
  let Rational(price_num_,price_den_) = swap_price_

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

  // The beacon_id must be the policy id of the beacon policy.
  trace_if_false( @"Wrong beacon_id", beacon_id == beacon_id_ ) &&

  // The offer asset must be a different asset than the ask asset.
  trace_if_false( @"Offer asset cannot be same as ask asset", offer != ask ) &&

  // The pair_beacon must be the sha2_256 hash of offer ++ ask.
  trace_if_false( 
    @"Wrong pair_beacon", 
    pair_beacon_ == generate_pair_beacon(offer,ask)
  ) &&

  // The offer_beacon must be the sha2_256 hash of offer_id ++ offer_name.
  trace_if_false( 
    @"Wrong offer_beacon", 
    offer_beacon_ == generate_offer_beacon(offer_id_,offer_name_)
  ) &&

  // The ask_beacon must be the sha2_256 hash of ask_id ++ ask_name.
  trace_if_false( 
    @"Wrong ask_beacon", 
    ask_beacon_ == generate_ask_beacon(ask_id_,ask_name_)
  ) &&

  // The swap_price denominator must be > 0.
  trace_if_false( 
    @"swap_price denominator not > 0", 
    price_den_ > 0 
  ) &&

  // The swap_price must be > 0.
  trace_if_false(
    @"swap_price numerator not > 0", 
    price_num_ > 0 
  ) &&

  // This is just to force the extract_ask_and_offer_quantity function to be executed.
  o >= 0
}

// Check if the beacons are locked at the proper address and the UTxO is configured properly.
// This function will return `True` or `False`. It uses `app_name` in the error message to generate
// a unique beacon policy for testing. 
pub fn beacon_destination_check(
  dapp_hash: Hash<Blake2b_224, Script>,
  beacon_id: PolicyId,
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The beacons must go to an address protected by the DApp validator script.
  // 2) The beacons must go to an address using a valid staking credential.
  // 3) The UTxOs with the beacons must have the proper value:
  //      a) Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
  //      b) The beacons must correspond to the beacons in the datum.
  //      c) There must be exactly 1 of each beacon.
  //      d) No extraneous assets are in the UTxO.
  // 4) The beacons must be stored with the proper inline `SwapDatum`:
  //      a) `beacon_id` == this policy id.
  //      b) `pair_beacon` == `sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name)`
  //      c) `offer_id` == policy id of the offer asset.
  //      d) `offer_name` == asset name of the offer asset.
  //      e) `offer_beacon` == `sha2_256("01" ++ offer_id ++ offer_name)`.
  //      f) `ask_id` == policy id of the ask asset.
  //      g) `ask_name` == asset name of the ask asset.
  //      h) `ask_beacon` == `sha2_256("02" ++ ask_id ++ ask_name)`
  //      i) `swap_price` denominator > 0
  //      j) `swap_price` numerator > 0
  // 5) The offer asset and the ask asset must be different assets.

  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the beacons in this UTxO and save it as a list for pattern matching.
    let beacon_list = value.tokens(val,beacon_id) |> dict.to_list()

    when beacon_list is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has three types of beacon.
      [(_,_),(_,_),(_,_)] -> {
        // All previous outputs must be valid.
        acc &&

        // The output must be at a DApp address with staking. Uses `app_name` in 
        // the error message.
        trace_if_false( 
          concat(@"Beacon must go to a ", app_name) |> concat(_,@" DApp address with staking"),
          pay == ScriptCredential(dapp_hash) && is_some(stake)
        ) &&

        // The output must be properly configured. This has its own trace messages.
        valid_swap_output(beacon_id,val,d)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"One-way swaps must have exactly three kinds of beacons"
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
    [Output(addr,oval,d,_), ..rest] -> {
      // Requirements for a valid corresponding output:
      // 1) The output is to the same swap address where the input originates.
      // 2) The output has exactly one of each of the proper beacons.
      // 3) The output's datum is exactly the same accept the `prev_input` == `Some(input_ref)`.

      // Check if the datum is correct. This is done first to rule out all but the target UTxO.
      // The rest of the checks will just check if the UTxO is properly configured.
      if req_datum == d {
        // Check if the output is to this swap address.
        if swap_addr == addr {
          // Get the necessary values.
          extract_ask_and_offer_quantity(
            oval,
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
      Some(input_ref)
    ) |> InlineDatum

  let Rational(price_num,price_den) = swap_price

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
    offer_taken * price_num <= ask_given * price_den
  ) &&

  // The ask asset cannot be taken. This would allow the swap to go in reverse.
  trace_if_false(
    @"The ask asset cannot be taken from the swap",
    ask_given >= 0 // Negative would mean it was taken.
  )
}
