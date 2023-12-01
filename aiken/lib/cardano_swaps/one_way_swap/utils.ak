use aiken/bytearray
use aiken/dict
use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/math/rational.{Rational} as ratio
use aiken/option.{is_some}
use aiken/string.{concat}
use aiken/transaction.{Datum,InlineDatum,Output,OutputReference} as tx
use aiken/transaction/credential.{
  Address,
  Script,
  ScriptCredential,
  }
use aiken/transaction/value.{PolicyId,AssetName,Value}

use cardano_swaps/common/types.{app_name,AssetConfig} as common_types
use cardano_swaps/common/utils.{
  error_if_false,
  trace_if_false,
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

// This function should only be called after the output has been checked for exactly three types of 
// beacons.
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
    ratio.denominator(swap_price_) > 0 
  ) &&

  // The swap_price must be > 0.
  trace_if_false(
    @"swap_price not > 0", 
    ratio.compare(swap_price_,ratio.zero()) == Greater 
  ) &&

  // The UTxO must have exactly 1 pair_beacon.
  trace_if_false(
    @"UTxO does not have exactly 1 pair_beacon",
    value.quantity_of( output_value, beacon_id, pair_beacon_ ) == 1
  ) &&

  // The UTxO must have exactly 1 offer_beacon.
  trace_if_false(
    @"UTxO does not have exactly 1 offer_beacon",
    value.quantity_of( output_value, beacon_id, offer_beacon_ ) == 1
  ) &&

  // The UTxO must have exactly 1 ask_beacon.
  trace_if_false(
    @"UTxO does not have exactly 1 ask_beacon",
    value.quantity_of( output_value, beacon_id, ask_beacon_ ) == 1
  ) &&

  // No extraneous assets can be stored in the UTxO.
  trace_if_false(
    @"No extraneous assets can be stored in the swap UTxO",
    list.foldl(
      value.flatten(output_value),
      True,
      fn(x: (PolicyId,AssetName,Int), acc: Bool) {
        let (cn,tn,_) = x
        // There is no need to check the beacon name since this UTxO was already checked
        // for duplicate beacons.
        acc && 
          ( cn == beacon_id  // It is a beacon.
         || (cn == offer_id_ && tn == offer_name_) // It is the offer asset.
         || (cn == ask_id_ && tn == ask_name_) // It is the ask asset.
         || cn == value.ada_policy_id // It is ada.
          )
      }
    )
  ) 
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
  //      j) `swap_price` > 0
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

// Calculate the swap flux for this specific Swap UTxO. The value is compared againts an output
// Swap UTxO with the exact same datum except the `prev_input` field is set to the current
// input's `OutputReference`. Negative values mean the swap gained the asset while positive 
// values mean the swap lost that asset. This is a recursive function that will terminate when the
// corresponding output is found or when no corresponding output is found. When no corresponding 
// output is found, the function will throw an error.
fn swap_value_diff(
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
  outputs: List<Output>
) -> Value {
  when outputs is {
    [] -> error @"Corresponding swap output not found"
    [Output(addr,oval,d,_), ..rest] -> {
      // Requirements for a valid corresponding output:
      // 1) The output is to the same swap address where the input originates.
      // 2) The output has exactly one of the proper trading pair beacon.
      // 3) The output's datum is exactly the same accept the `prev_input` == `Some(input_ref)`.

      // Check if the output has the beacon for this trading pair. It must have exactly one.
      if value.quantity_of( oval, beacon_id, pair_beacon ) == 1 {
        // Check if the output is to this swap address.
        if swap_addr == addr {
          // Check if the datum is correct.
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
            prev_input
          ) = parse_datum(d)

          // Check if the prev_input field points to this input.
          if prev_input == Some(input_ref) {
            // The beacon_id must be the policy id of the beacon policy.
            expect True = error_if_false( 
              @"Wrong beacon_id", 
              beacon_id == beacon_id_ 
            )

            // The pair_beacon must be the pair_beacon from the input.
            expect True = error_if_false( 
              @"Wrong pair_beacon", 
              pair_beacon == pair_beacon_ 
            )

            // The offer_id must be the offer_id from the input.
            expect True = error_if_false( 
              @"Wrong offer_id", 
              offer_id == offer_id_
            )

            // The offer_name must be the offer_name from the input.
            expect True = error_if_false( 
              @"Wrong offer_name", 
              offer_name == offer_name_
            )

            // The offer_beacon must be the offer_beacon from the input.
            expect True = error_if_false( 
              @"Wrong offer_beacon", 
              offer_beacon == offer_beacon_
            )

            // The ask_id must be the ask_id from the input.
            expect True = error_if_false( 
              @"Wrong ask_id", 
              ask_id == ask_id_
            )

            // The ask_name must be the ask_name from the input.
            expect True = error_if_false( 
              @"Wrong ask_name", 
              ask_name == ask_name_
            )

            // The ask_beacon must be the ask_beacon from the input.
            expect True = error_if_false( 
              @"Wrong ask_beacon", 
              ask_beacon == ask_beacon_
            )

            // The swap_price must be the swap_price from the input.
            expect True = error_if_false( 
              @"Wrong swap_price", 
              swap_price == swap_price_
            )

            // Subtract the output value from the input. This stops the recursion.
            value.merge( input_val, value.negate(oval) )
          }

          // This cannot be the corresponding output so recurse.
          else {
            swap_value_diff(
              input_ref,
              input_val,
              swap_addr,
              beacon_id,
              pair_beacon,
              offer_id,
              offer_name,
              offer_beacon,
              ask_id,
              ask_name,
              ask_beacon,
              swap_price,
              rest
            )
          }
        }

        // This cannot be the corresponding output so recurse.
        else {
          swap_value_diff(
            input_ref,
            input_val,
            swap_addr,
            beacon_id,
            pair_beacon,
            offer_id,
            offer_name,
            offer_beacon,
            ask_id,
            ask_name,
            ask_beacon,
            swap_price,
            rest
          )
        }
      }
      
      // This cannot be the corresponding output so recurse.
      else {
        swap_value_diff(
          input_ref,
          input_val,
          swap_addr,
          beacon_id,
          pair_beacon,
          offer_id,
          offer_name,
          offer_beacon,
          ask_id,
          ask_name,
          ask_beacon,
          swap_price,
          rest
        )
      }
    }
  }
}

// Check that only the offered asset leaves during the swap. It also checks that a swap only
// gained the ask asset or ada. This helps optimize the beacon querying by contraining UTxO
// assets to just the relevant assets. 
fn only_proper_assets_swapped(
  flux: Value, 
  offer_id: PolicyId, 
  offer_name: AssetName, 
  ask_id: PolicyId, 
  ask_name: AssetName
) -> Bool {
  // Function to map over the value and check that only the offered asset is positive.
  // Positive values mean the asset was taken.
  let foo = fn (asset_flux: (PolicyId,AssetName,Int), acc: Bool) {
    // Extract the information from the asset_flux.
    let (cn,tn,n) = asset_flux

    // If n > 0, that asset must be the offered asset.
    if n > 0 {
      // The previous assets must also be correctly managed.
      acc && trace_if_false( 
        @"Only the offered asset can leave the swap",
        cn == offer_id && tn == offer_name
      )
    } 
    
    // If n <= 0, then the asset must be the asked asset or ada (if the minUTxO value increased).
    else {
      acc && trace_if_false(
        @"Only the asked asset or ada can be deposited into the swap",
        (cn == ask_id && tn == ask_name) || cn == value.ada_policy_id
      )
    }
  }

  // Map over the list of assets in flux.
  list.foldl(
    value.flatten(flux), // Convert the `Value` to `List<(PolicyId,AssetName,Int)>`
    True,
    foo
  )
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
  // Get the swap flux for this address.
  let swap_flux = swap_value_diff(
    input_ref,
    input_val,
    swap_addr,
    beacon_id,
    pair_beacon,
    offer_id,
    offer_name,
    offer_beacon,
    ask_id,
    ask_name,
    ask_beacon,
    swap_price,
    outputs
  )

  // Calculate the quantity of the ask asset in flux.
  let ask_given = 
    value.quantity_of( swap_flux, ask_id, ask_name )
    |> fn(x) {-x} // Must negate the number since negative means the asset was received.
    |> ratio.from_int(_) // Convert it to a `Rational`.

  // Calculate the quantity of the offer asset in flux.
  let offer_taken =
    value.quantity_of( swap_flux, offer_id, offer_name )
    |> ratio.from_int(_) // Convert it to a `Rational`.

  // Compare the ask and offer quantities.
  let swap_ratio = 
    ratio.mul(offer_taken, swap_price) // Convert the offer asset to units of the ask asset.
    |> ratio.compare(_, ask_given) // Compare it against the amount of ask given.

  // The swap_ratio must be `Less` or `Equal`. Using `!= Greater` had a performance regression...
  trace_if_false( 
    @"Fail: offer_taken * price <= ask_given",
    swap_ratio == Less || swap_ratio == Equal 
  ) &&

  // Only the offer asset can leave and only the ask asset can be deposited. 
  // This has its own error message.
  only_proper_assets_swapped( swap_flux, offer_id, offer_name, ask_id, ask_name )
}
