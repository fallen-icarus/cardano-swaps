use aiken/bytearray
use aiken/dict
use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/math/rational.{Rational} as ratio
use aiken/option.{is_some}
use aiken/string.{concat}
use aiken/transaction.{Datum,InlineDatum,Input,Output} as tx
use aiken/transaction/credential.{
  Address,
  Script,
  ScriptCredential,
  }
use aiken/transaction/value.{PolicyId,AssetName,MintedValue,Value}

use cardano_swaps/common/types.{app_name,AssetConfig} as common_types
use cardano_swaps/common/utils.{
  error_if_false,
  trace_if_false
  } as common_utils
use cardano_swaps/one_way_swap/types.{
  BeaconRedeemer,
  BurnBeacons,
  CreateSwap,
  SwapDatum,
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

// Create the asset name for the beacon by hashing the combination of the policy id and asset name.
// It uses the sha2_256 hash algorithm.
pub fn generate_beacon_name(asset: AssetConfig) -> AssetName {
  let AssetConfig(policy_id,asset_name) = asset
  hash.sha2_256(
    bytearray.concat( policy_id, asset_name )
  )
}

// Check if the owner stores the beacons in a UTxO with a proper inline SwapDatum. This will crash 
// the script if the `SwapDatum` is not an inline datum. It is used for both `CreateSwap` and 
// `CloseOrUpdate`.
fn valid_swap_datum_creation(
  beacon_id: PolicyId,
  beacon_name: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName,
  datum: Datum
) -> Bool {
  // Try to parse the datum and extract out the information. This will crash the script if the
  // datum is not an inline `SwapDatum`.
  let SwapDatum( 
    beacon_id_, 
    beacon_name_,
    offer_id_,
    offer_name_,
    ask_id_,
    ask_name_,
    price
  ) = parse_datum(datum)

  // The beacon_id must be the policy id of the beacon policy.
  trace_if_false( @"Invalid SwapDatum beacon_id", beacon_id == beacon_id_ ) &&

  // The beacon name must be sha2_256( ask_asset policy id ++ ask_asset asset name )
  trace_if_false( @"Invalid SwapDatum beacon_name", beacon_name == beacon_name_ ) &&

  // The offer_id must be the offer_id that this beacon policy is for.
  trace_if_false( @"Invalid SwapDatum offer_id", offer_id == offer_id_ ) &&

  // The offer_name must be the offer_name that this beacon policy is for.
  trace_if_false( @"Invalid SwapDatum offer_name", offer_name == offer_name_ ) &&

  // The ask_id must be the ask_id for this beacon.
  trace_if_false( @"Invalid SwapDatum ask_id", ask_id == ask_id_ ) &&

  // The ask_name must be the ask_name for this beacon.
  trace_if_false( @"Invalid SwapDatum ask_name", ask_name == ask_name_ ) &&

  // The price denominator must be > 0.
  trace_if_false( @"Swap price denominator not > 0", ratio.denominator(price) > 0 ) &&

  // The price must be > 0.
  trace_if_false( @"Swap price not > 0", ratio.compare(price,ratio.zero()) == Greater )
}

// Check whether the proper beacons are minted/burned based on the redeemer. It returns `True` or 
// `False`.
pub fn mint_check(
  beacon_id: PolicyId,
  beacon_names: List<AssetName>,
  redeemer: BeaconRedeemer, 
  mint: MintedValue
) -> Bool {
  // Get all the beacons minted/burned in this transaction.
  let beacons_minted = 
    value.from_minted_value(mint)
    |> value.tokens(_,beacon_id)

  when redeemer is {
    CreateSwap(_) -> {
      // Checks:
      // 1) Only the beacon for the target asset can be minted.
      // 2) The token name must be: sha2_256( ask_id ++ ask_name ).
      
      // Convert the dictionary to a list and extract the first element in each tuple.
      let names = 
        dict.to_list(beacons_minted) 
        |> list.map(_, fn(x) { let (f,_) = x f })

      // Only the target beacons can be minted.
      trace_if_false (
        @"Only the beacons in the redeemer can be minted/burned",
        list.sort(names,bytearray.compare) == list.sort(beacon_names,bytearray.compare)
      )

      // Beacons can be minted or burned with this redeemer. This is useful for moving assets
      // from one swap into another. The destination for all minted beacons will still be
      // checked. Any burned beacons must still be present in the redeemer.
    }

    BurnBeacons -> {
      // Always allowed as long as this redeemer is only used to burn.
      trace_if_false( 
        @"This redeemer can only be used to burn",
        dict.foldl( beacons_minted, True, fn(_,v,r) {r && v < 0})
      )
    }
  }
}

// Check if the beacons are minted to the proper address and the UTxO is configured properly.
// This function will return `True` or `False`. It uses `app_name` in the error message to generate
// a unique beacon policy for testing.
pub fn beacon_destination_check(
  dapp_hash: Hash<Blake2b_224, Script>,
  beacon_id: PolicyId,
  offer_id: PolicyId,
  offer_name: AssetName,
  beacon_names: List<AssetName>,
  ask_assets: List<AssetConfig>,
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The beacons must go to an addres protected by the dApp validator script.
  // 2) The beacons must go to an address using a valid staking credential.
  // 3) The beacons must be stored individually at the dApp address.
  // 4) The beacons must be stored with the proper inline `SwapDatum`:
  //      a) `beacon_id` == this policy id.
  //      b) `beacon_name` == sha2_256( ask_asset policy id ++ ask_asset asset name )
  //      c) `offer_id` == asset_id of offer_config
  //      d) `offer_name` == asset_name of offer_config
  //      e) `ask_id` == asset_id of ask_asset
  //      f) `ask_name` == asset_name of ask_asset
  //      g) `price` denominator > 0
  //      h) `price` > 0
  // 5) The beacons must be stored with some of the offered asset.

  // Create a map from beacon names to `AssetConfig` (information from redeemer).
  let xs = list.zip(beacon_names,ask_assets) |> dict.from_list(_,bytearray.compare)

  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the beacons in this UTxO and save it as a list for pattern matching.
    let beacon_list = value.tokens(val,beacon_id) |> dict.to_list()

    when beacon_list is {
      // The UTxO has one type of beacon.
      [(beacon_name,beacon_num)] -> {
        // Check if the beacon in this output is one of the newly minted ones. If it is, it would
        // be found in the redeemer (this redeemer only allows minting beacons found in the
        // redeemer). If it isn't found in the redeemer, then this beacon must have come from an
        // input. That is only possible if this redeemer is composed with either the `CloseOrUpdate`
        // spending redeemer or the `Swap` spending redeemer. Both of the spending redeemers
        // check that the beacons are stored properly so there is no need for the minting policy
        // to check the destination of beacons from inputs. Therefore, the minting policy
        // only needs to check the destination of newly minted beacons.

        when dict.get(xs,beacon_name) is {
          None -> acc // The destination of this beacon is checked by the spending redeemer.

          // This is a newly minted beacon and the destination needs to be checked.
          Some(AssetConfig(ask_id,ask_name)) -> {
            // Get a list of all assets in this UTxO. This will be used to check that no
            // extraneous assets are present.
            let assets_present = value.flatten(val)
            
            // The beacon must be stored at a dapp address with staking. Uses `app_name` in 
            // the error message.
            trace_if_false( 
              concat(@"Beacon must go to a ", app_name) |> concat(_,@" dApp address with staking"),
              pay == ScriptCredential(dapp_hash) && is_some(stake)
            ) &&

            // The beacons must be stored individually.
            trace_if_false(
              @"Beacons must be stored individually",
              beacon_num == 1
            ) &&

            // The beacons must be stored with some of the offered asset.
            trace_if_false(
              @"Beacons must be stored with some of the offered asset",
              value.quantity_of( val, offer_id, offer_name ) > 0
            ) &&

            // No extraneous assets can be stored in the UTxO.
            trace_if_false(
              @"No extraneous assets can be stored in the swap UTxO",
              list.foldl(
                assets_present,
                True,
                fn(x: (PolicyId,AssetName,Int), acc: Bool) {
                  let (cn,tn,_) = x
                  // There is no need to check the beacon name since this UTxO was already checked
                  // for duplicate beacons.
                  acc && 
                    ( cn == beacon_id  // It is a beacon.
                   || (cn == offer_id && tn == offer_name) // It is the offer asset.
                   || cn == value.ada_policy_id // It is ADA.
                    )
                }
                )
            ) &&

            // The beacons must be stored with the proper inline SwapDatum. This function has its 
            // own error messages.
            valid_swap_datum_creation(
              beacon_id,
              beacon_name,
              offer_id,
              offer_name,
              ask_id,
              ask_name,
              d
            ) &&

            // All previously checked outputs must be valid.
            acc
          }
        }
      }

      // If beacons are not present, this output can be ignored.
      [] -> acc

      // This path can only be taken if the above two matches failed which means the UTxO
      // has multiple types of beacons in it.
      _ -> error @"Beacons for different trading pairs must get separate UTxOs"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl( outputs, True, foo )
}

// Checks whether the beacons are stored properly when the `CloseOrUpdate` redeemer is used.
pub fn valid_update_outputs(
  swap_datum: SwapDatum,
  outputs: List<Output>,
  swap_addr: Address
) -> Bool {
  // Checks:
  // 1) Any beacons not burned must be re-output to this address with the proper inline `SwapDatum`.
  //      a) all fields same as input datum except the `price`
  //      b) `price` denominator > 0
  //      c) `price` > 0
  // 2) The beacons must be stored with some of the offered asset.
  // 3) The beacons must be stored individually.
  // 4) No extraneous assets can be in the swap.

  // Extract out the information from the current datum.
  let SwapDatum(
    beacon_id, 
    beacon_name,
    offer_id,
    offer_name,
    ask_id,
    ask_name,
    _ // The price isn't needed since it can be replaced.
  ) = swap_datum

  // This function is used to map over the outputs to check validity.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(addr,val,d,_) = out

    // Get the number of beacons in this UTxO.
    let beacon_num = value.quantity_of( val, beacon_id, beacon_name )

    // If the UTxO contains a beacon, it must be checked:
    if beacon_num > 0 {
      // Get a list of all assets in this UTxO. This will be used to check that no
      // extraneous assets are present.
      let assets_present = value.flatten(val)

      // The beacon must be stored at this address.
      trace_if_false( 
        @"Beacons not stored at this swap address",
        addr == swap_addr
      ) &&

      // The beacons must be stored individually.
      trace_if_false(
        @"Beacons must be stored individually",
        beacon_num == 1
      ) &&

      // The beacons must be stored with some of the offered asset.
      trace_if_false(
        @"Beacons must be stored with some of the offered asset",
        value.quantity_of( val, offer_id, offer_name ) > 0
      ) &&

      // No extraneous assets can be stored in the UTxO.
      trace_if_false(
        @"No extraneous assets can be stored in the swap UTxO",
        list.foldl(
          assets_present,
          True,
          fn(x: (PolicyId,AssetName,Int), acc: Bool) {
            let (cn,tn,_) = x
            // There is no need to check the beacon name since every beacon will be forced to have
            // the proper datum.
            acc && 
              ( cn == beacon_id  // It is a beacon.
              || (cn == offer_id && tn == offer_name) // It is the offer asset.
              || cn == value.ada_policy_id // It is ADA.
              )
          }
          )
      ) &&

      // The beacons must be stored with the proper inline `SwapDatum`. This function has its own
      // error messages.
      valid_swap_datum_creation(
        beacon_id,
        beacon_name,
        offer_id,
        offer_name,
        ask_id,
        ask_name,
        d
      ) &&

      // All previously checked outputs must be valid.
      acc
    }

    // If beacons are not present, this output can be ignored.
    else {
      acc
    }

  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl( outputs, True, foo )
}

// Get the total input value for a specific trading pair from this swap address.
// The `Rational` returned is the weighted avg price of all the relevant inputs.
fn swap_input_info(
  beacon_id: PolicyId,
  beacon_name: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  swap_addr: Address,
  inputs: List<Input>
) -> (Value,Rational) {
  // This function maps over the inputs to build up the accumulator. The `Int` is a temp variable
  // to keep track of the running total. It is needed to calculate the weighted avg price.
  let foo = fn (in: Input, acc: (Value,Int,Rational)) {
    // Extract out the information from the input.
    let Input(_,Output(addr,ival,d,_)) = in

    // Extract out the current information from the accumulator.
    let (val,taken,wp) = acc

    // Check if the input has the beacon for this trading pair.
    if value.quantity_of( ival, beacon_id, beacon_name ) > 0 {
      // Check if the input is from this address.
      if addr == swap_addr {
        // Calculate the new total taken.
        let this_taken = value.quantity_of( ival, offer_id, offer_name ) 
        let new_taken = this_taken + taken

        // Calculate the new weighted price.
        let SwapDatum(_,_,_,_,_,_,price) = parse_datum(d)
        expect Some(prev_prop) = ratio.new( taken, new_taken ) // Will fail if new_taken == 0.
        expect Some(this_prop) = ratio.new( this_taken, new_taken ) // Will fail if new_taken == 0.
        let new_weighted_price =
          ratio.reduce(
            ratio.add(
              ratio.mul( prev_prop, wp ),
              ratio.mul( this_prop, price )
            )
          )

        // Calculate the new total value.
        let new_val = value.merge( val, ival )

        // Return the results.
        ( new_val, new_taken, new_weighted_price )
      }

      // If it is from a different address, this input can be ignored.
      else {
        acc
      }
    }

    // If no beacons are present, this input can be ignored.
    else {
      acc
    }
  }

  // Map over the inputs.
  let (val,_,wp) = list.foldl(
    inputs, 
    ( value.zero(), 0, ratio.zero()),
    foo
  )

  // Drop the Int and return the results.
  (val,wp)
}

// Calculate the swap flux for this specific trading pair from this address. Negative values mean 
// the address gained the asset while positive values mean the address lost that asset. This also 
// checks that any output to the swap address has the proper datum.
fn swap_value_diff(
  beacon_id: PolicyId,
  beacon_name: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName,
  req_price: Rational,
  outputs: List<Output>,
  total_input_val: Value,
  swap_addr: Address
) -> Value {
  // A function to map over the outputs and build up the value flux. It will also check that the
  // swap output is valid.
  let foo = fn (out: Output, acc: Value) {
    // Extract the information from the output.
    let Output(addr,oval,d,_) = out

    // Check if the output has the beacon for this trading pair.
    if value.quantity_of( oval, beacon_id, beacon_name ) > 0 {
      // Check if the input is from this address.
      if addr == swap_addr {
        // Parse the datum for this output. It must have an inline `SwapDatum`.
        let SwapDatum( 
          beacon_id_, 
          beacon_name_,
          offer_id_,
          offer_name_,
          ask_id_,
          ask_name_,
          price
        ) = parse_datum(d)

        // The beacon_id must be the policy id of the beacon policy.
        expect True = error_if_false( @"Invalid SwapDatum beacon_id", beacon_id == beacon_id_ )

        // The beacon name must be sha2_256( ask_asset policy id ++ ask_asset asset name )
        expect True = error_if_false( @"Invalid SwapDatum beacon_name", beacon_name == beacon_name_ )

        // The offer_id must be the offer_id that this beacon policy is for.
        expect True = error_if_false( @"Invalid SwapDatum offer_id", offer_id == offer_id_ )

        // The offer_name must be the offer_name that this beacon policy is for.
        expect True = error_if_false( @"Invalid SwapDatum offer_name", offer_name == offer_name_ )

        // The ask_id must be the ask_id for this beacon.
        expect True = error_if_false( @"Invalid SwapDatum ask_id", ask_id == ask_id_ )

        // The ask_name must be the ask_name for this beacon.
        expect True = error_if_false( @"Invalid SwapDatum ask_name", ask_name == ask_name_ )

        // The price must be the required price.
        expect True = error_if_false ( @"SwapDatum price not weighted avg", price == req_price )

        // Subtract the output value from the input.
        value.merge( acc, value.negate(oval) )
      }

      // If the output is to a different address, ignore it.
      else {
        acc
      }
    }

    // If it does not have any beacons, ignore this output.
    else {
      acc
    }
  }

  // Map the function over the outputs.
  list.foldl( outputs, total_input_val, foo )
}

// Check that only the offered asset leaves during the swap. It also checks that a swap only
// gained the ask asset. This helps optimize the beacon querying by contraining UTxO
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

// Compare the inputs and outputs to check that the swap is valid.
pub fn valid_swap(
  beacon_id: PolicyId,
  beacon_name: AssetName,
  offer_id: PolicyId,
  offer_name: AssetName,
  ask_id: PolicyId,
  ask_name: AssetName,
  outputs: List<Output>,
  inputs: List<Input>,
  swap_addr: Address
) -> Bool {
  // Get the swap input information.
  let (swap_input_value, weighted_price) = swap_input_info(
    beacon_id,
    beacon_name,
    offer_id,
    offer_name,
    swap_addr,
    inputs
  )

  // Get the swap flux for this address.
  let swap_flux = swap_value_diff(
    beacon_id,
    beacon_name,
    offer_id,
    offer_name,
    ask_id,
    ask_name,
    weighted_price,
    outputs,
    swap_input_value,
    swap_addr
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
    ratio.mul(offer_taken, weighted_price) // Convert the offer asset to units of the ask asset.
    |> ratio.compare(_, ask_given) // Compare it against the amount of ask given.

  // The swap_ratio must be `Less` or `Equal`. Using `!= Greater` had a performance regression...
  trace_if_false( 
    @"Fail: offer_taken * weighted_price <= ask_given",
    swap_ratio == Less || swap_ratio == Equal 
  ) &&

  // Only the offer asset can leave and only the ask asset can be deposited. 
  // This has its own error message.
  only_proper_assets_swapped( swap_flux, offer_id, offer_name, ask_id, ask_name )
}
