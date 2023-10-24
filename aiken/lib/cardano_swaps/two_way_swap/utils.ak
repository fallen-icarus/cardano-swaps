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
  compare_asset_config,
  error_if_false,
  trace_if_false,
  generate_pair_beacon,
  generate_offer_beacon
  } as common_utils
use cardano_swaps/two_way_swap/types.{
  SwapDatum
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

// This function is used for `CloseOrUpdate` and `CreateSwap`. This function should only be called
// after the output has been checked for exactly three types of beacons.
fn valid_swap_output(
  beacon_id: PolicyId,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  let SwapDatum(
    beacon_id_,
    pair_beacon_,
    asset1_id_,
    asset1_name_,
    asset1_beacon_,
    asset2_id_,
    asset2_name_,
    asset2_beacon_,
    forward_price,
    reverse_price,
    _ // prev_input does not need to be check.
  ) = parse_datum(output_datum)

  let asset1 = AssetConfig(asset1_id_,asset1_name_)
  let asset2 = AssetConfig(asset2_id_,asset2_name_)

  // The beacon_id must be the policy id of the beacon policy.
  trace_if_false( @"Wrong beacon_id", beacon_id == beacon_id_ ) &&

  // The trading pair must be sorted: asset1 < asset2. They cannot be equal.
  trace_if_false( 
    @"Asset1 must be less than asset2", 
    compare_asset_config(asset1,asset2) == Less
  ) &&

  // The pair_beacon must be the sha2_256 hash of asset1 ++ asset2.
  trace_if_false( 
    @"Wrong pair_beacon", 
    pair_beacon_ == generate_pair_beacon(asset1,asset2)
  ) &&

  // The asset1_beacon must be the sha2_256 hash of assetId ++ assetName.
  trace_if_false( 
    @"Wrong asset1_beacon", 
    asset1_beacon_ == generate_offer_beacon(asset1_id_,asset1_name_)
  ) &&

  // The asset2_beacon must be the sha2_256 hash of assetId ++ assetName.
  trace_if_false( 
    @"Wrong asset2_beacon", 
    asset2_beacon_ == generate_offer_beacon(asset2_id_,asset2_name_)
  ) &&

  // The forward_price denominator must be > 0.
  trace_if_false( 
    @"forward_price denominator not > 0", 
    ratio.denominator(forward_price) > 0 
  ) &&

  // The forward_price must be > 0.
  trace_if_false(
    @"forward_price not > 0", 
    ratio.compare(forward_price,ratio.zero()) == Greater 
  ) &&

  // The reverse_price denominator must be > 0.
  trace_if_false( 
    @"reverse_price denominator not > 0", 
    ratio.denominator(reverse_price) > 0 
  ) &&

  // The reverse_price must be > 0.
  trace_if_false( 
    @"reverse_price not > 0", 
    ratio.compare(reverse_price,ratio.zero()) == Greater 
  ) &&

  // The UTxO must have exactly 1 pair_beacon.
  trace_if_false(
    @"UTxO does not have exactly 1 pair_beacon",
    value.quantity_of( output_value, beacon_id, pair_beacon_ ) == 1
  ) &&

  // The UTxO must have exactly 1 asset1_beacon.
  trace_if_false(
    @"UTxO does not have exactly 1 asset1_beacon",
    value.quantity_of( output_value, beacon_id, asset1_beacon_ ) == 1
  ) &&

  // The UTxO must have exactly 1 asset2_beacon.
  trace_if_false(
    @"UTxO does not have exactly 1 asset2_beacon",
    value.quantity_of( output_value, beacon_id, asset2_beacon_ ) == 1
  ) &&

  // The beacons must be stored with asset1 and/or asset2.
  trace_if_false(
    @"Beacons must be stored with asset1 and/or asset2",
    value.quantity_of( output_value, asset1_id_, asset1_name_ ) > 0 ||
    value.quantity_of( output_value, asset2_id_, asset2_name_ ) > 0
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
         || (cn == asset1_id_ && tn == asset1_name_) // It is asset1.
         || (cn == asset2_id_ && tn == asset2_name_) // It is asset2.
         || cn == value.ada_policy_id // It is ADA.
          )
      }
    )
  ) 
}

// Check if the beacons are locked at the proper address and the UTxO is configured properly.
// This function will return `True` or `False`. It uses `app_name` in the error message to generate
// a unique beacon policy for testing. This function is used for both `CloseOrUpdate` and 
// `CreateSwap`.
pub fn beacon_destination_check(
  dapp_hash: Hash<Blake2b_224, Script>,
  beacon_id: PolicyId,
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The beacons must go to an addres protected by the dApp validator script.
  // 2) The beacons must go to an address using a valid staking credential.
  // 3) The UTxOs with the beacons must have the proper value:
  //      a) Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
  //      b) The beacons must correspond to the beacons in the datum.
  //      c) There must be exactly 1 of each beacon.
  //      d) The UTxO must have asset1 and/or asset2.
  //      e) No extraneous assets are in the UTxO.
  // 4) The beacons must be stored with the proper inline `SwapDatum`:
  //      a) `beacon_id` == this policy id.
  //      b) `pair_beacon` == sha2_256(asset1_id ++ asset1_name ++ asset2_id ++ asset2_name)
  //      c) `asset1_id` == asset_id of asset1 for that trading pair.
  //      d) `asset1_name` == asset_name of asset1 for that trading_pair.
  //      e) `asset1_beacon` == sha2_256(asset1_id ++ asset1_name).
  //      f) `asset2_id` == asset_id asset2 for that trading_pair.
  //      g) `asset2_name` == asset_name asset2 for that trading_pair.
  //      h) `asset2_beacon` == sha2_256(asset2_id ++ asset2_name).
  //      i) `forward_price` denominator > 0
  //      j) `forward_price` > 0
  //      k) `reverse_price` denominator > 0
  //      l) `reverse_price` > 0
  //      m) asset1 < asset2
  //
  // The validator assumes the trading pairs are sorted which is why asset1 must be less than
  // asset2. Asset1 and asset2 cannot be the same asset.

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

        // The output must be at a dApp address with staking. Uses `app_name` in 
        // the error message.
        trace_if_false( 
          concat(@"Beacon must go to a ", app_name) |> concat(_,@" dApp address with staking"),
          pay == ScriptCredential(dapp_hash) && is_some(stake)
        ) &&

        // The output must be properly configured. This has its own trace messages.
        valid_swap_output(beacon_id,val,d)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Two-way swaps must have exactly three kinds of beacons"
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
  asset1_id: PolicyId,
  asset1_name: AssetName,
  asset1_beacon: AssetName,
  asset2_id: PolicyId,
  asset2_name: AssetName,
  asset2_beacon: AssetName,
  forward_price: Rational,
  reverse_price: Rational,
  outputs: List<Output>
) -> Value {
  when outputs is {
    [] -> error @"Corresponding swap output not found"
    [Output(addr,oval,d,_), ..rest] -> {
      // Requirements for a valid corresponding output:
      // 1) The output is to the same swap address where the input originates.
      // 2) The output has exactly one of the proper beacon.
      // 3) The output's datum is exactly the same accept the `prev_input` == `Some(input_ref)`.

      // Check if the output has the beacon for this trading pair. It must have exactly one.
      if value.quantity_of( oval, beacon_id, pair_beacon ) == 1 {
        // Check if the output is to this swap address.
        if swap_addr == addr {
          // Check if the datum is correct.
          let SwapDatum(
            beacon_id_,
            pair_beacon_,
            asset1_id_,
            asset1_name_,
            asset1_beacon_,
            asset2_id_,
            asset2_name_,
            asset2_beacon_,
            forward_price_,
            reverse_price_,
            prev_input
          ) = parse_datum(d)

          // Check if the prev_input field points to this input.
          if prev_input == Some(input_ref) {
            // The beacon_id must be the policy id of the beacon policy.
            expect True = error_if_false( 
              @"Wrong beacon_id", 
              beacon_id == beacon_id_ 
            )

            // The beacon_name must be the beacon_name from the input.
            expect True = error_if_false( 
              @"Wrong pair_beacon", 
              pair_beacon == pair_beacon_ 
            )

            // The asset1_id must be the asset1_id from the input.
            expect True = error_if_false( 
              @"Wrong asset1_id", 
              asset1_id == asset1_id_ 
            )

            // The asset1_name must be the asset1_name from the input.
            expect True = error_if_false( 
              @"Wrong asset1_name", 
              asset1_name == asset1_name_ 
            )

            // The asset1_beacon must be the asset1_beacon from the input.
            expect True = error_if_false( 
              @"Wrong asset1_beacon", 
              asset1_beacon == asset1_beacon_ 
            )


            // The asset2_id must be the asset2_id from the input.
            expect True = error_if_false( 
              @"Wrong asset2_id", 
              asset2_id == asset2_id_ 
            )

            // The asset2_name must be the asset2_name from the input.
            expect True = error_if_false( 
              @"Wrong asset2_name", 
              asset2_name == asset2_name_ 
            )

            // The asset2_beacon must be the asset2_beacon from the input.
            expect True = error_if_false( 
              @"Wrong asset2_beacon", 
              asset2_beacon == asset2_beacon_ 
            )

            // The forward_price must be the forward_price from the input.
            expect True = error_if_false( 
              @"Wrong forward_price", 
              forward_price == forward_price_
            )

            // The reverse_price must be the reverse_price from the input.
            expect True = error_if_false( 
              @"Wrong reverse_price", 
              reverse_price == reverse_price_
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
              asset1_id,
              asset1_name,
              asset1_beacon,
              asset2_id,
              asset2_name,
              asset2_beacon,
              forward_price,
              reverse_price,
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
            asset1_id,
            asset1_name,
            asset1_beacon,
            asset2_id,
            asset2_name,
            asset2_beacon,
            forward_price,
            reverse_price,
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
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          forward_price,
          reverse_price,
          rest
        )
      }
    }
  }
}

// Check that only the offered asset leaves during the swap. It also checks that a swap only
// gained the ask asset. This helps optimize the beacon querying by contraining UTxO
// assets to just the relevant assets. When `ForwardSwap` is used, Asset1 is the ask asset
// and Asset2 is the offer asset. When `ReverseSwap` is used, Asset2 is the ask asset and Asset1
// is the offer asset.
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
    
    // If n <= 0, then the asset must be the asked asset or ADA (if the minUTxO value increased).
    else {
      acc && trace_if_false(
        @"Only the asked asset or ADA can be deposited into the swap",
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
  asset1_id: PolicyId,
  asset1_name: AssetName,
  asset1_beacon: AssetName,
  asset2_id: PolicyId,
  asset2_name: AssetName,
  asset2_beacon: AssetName,
  forward_price: Rational,
  reverse_price: Rational,
  outputs: List<Output>,
  reverse: Bool // Whether the `ReverseSwap` redeemer was used. Simpler than passing the redeemer.
) -> Bool {
  // Get the swap flux for this address.
  let swap_flux = swap_value_diff(
    input_ref,
    input_val,
    swap_addr,
    beacon_id,
    pair_beacon,
    asset1_id,
    asset1_name,
    asset1_beacon,
    asset2_id,
    asset2_name,
    asset2_beacon,
    forward_price,
    reverse_price,
    outputs
  )

  // Which asset is ask/offer and what price should be used.
  let (offer_id,offer_name,ask_id,ask_name,price) = 
    if reverse { (asset1_id,asset1_name,asset2_id,asset2_name,reverse_price) }
    else { (asset2_id,asset2_name,asset1_id,asset1_name,forward_price) }

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
    ratio.mul(offer_taken, price) // Convert the offer asset to units of the ask asset.
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
