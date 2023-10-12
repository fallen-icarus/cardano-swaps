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
use aiken/transaction/value.{PolicyId,AssetName,MintedValue,Value}

use cardano_swaps/common/types.{app_name,AssetConfig} as common_types
use cardano_swaps/common/utils.{
  error_if_false,
  trace_if_false
  } as common_utils
use cardano_swaps/two_way_swap/types.{
  BeaconRedeemer,
  BurnBeacons,
  CreateSwap,
  SwapDatum,
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

pub fn compare_asset_config(asset1: AssetConfig, asset2: AssetConfig) -> Ordering {
  let AssetConfig(asset1_id,asset1_name) = asset1
  let AssetConfig(asset2_id,asset2_name) = asset2
  when bytearray.compare(asset1_id,asset2_id) is {
    Less -> Less
    Greater -> Greater
    Equal -> bytearray.compare(asset1_name,asset2_name)
  }
}

// In order to ensure that there is only one beacon for each trading pair, the beacon name
// must be independent of the pair's ordering. This function sorts all the pairs by name.
// Asset1 will always be the first asset in the sorted pair; Asset2 will always be the second one.
pub fn sort_pair(pair: (AssetConfig,AssetConfig)) -> (AssetConfig,AssetConfig) {
  let (asset1,asset2) = pair
  when compare_asset_config(asset1,asset2) is {
    Greater -> (asset2,asset1)
    _ -> pair
  }
}

// Create the asset name for the beacon by hashing the combination of the policy id and asset name.
// It uses the sha2_256 hash algorithm. 
pub fn generate_beacon_name(pair: (AssetConfig,AssetConfig)) -> AssetName {
  let (AssetConfig(asset1_id,asset1_name),AssetConfig(asset2_id,asset2_name)) = pair
  hash.sha2_256(
    bytearray.concat(asset1_id,asset1_name)
    |> bytearray.concat(_,asset2_id)
    |> bytearray.concat(_,asset2_name)
  )
}

// Check if the owner stores the beacons in a UTxO with a proper inline `SwapDatum`. This will crash 
// the script if the `SwapDatum` is not an inline datum. It is used for both `CreateSwap` and 
// `CloseOrUpdate`.
fn valid_swap_datum_creation(
  beacon_id: PolicyId,
  beacon_name: AssetName,
  asset1_id: PolicyId,
  asset1_name: AssetName,
  asset2_id: PolicyId,
  asset2_name: AssetName,
  datum: Datum
) -> Bool {
  // Try to parse the datum and extract out the information. This will crash the script if the
  // datum is not an inline `SwapDatum`.
  let SwapDatum( 
    beacon_id_, 
    beacon_name_,
    asset1_id_,
    asset1_name_,
    asset2_id_,
    asset2_name_,
    forward_price,
    reverse_price,
    _
  ) = parse_datum(datum)

  // The beacon_id must be the policy id of the beacon policy.
  trace_if_false( @"Invalid SwapDatum beacon_id", beacon_id == beacon_id_ ) &&

  // The beacon_name must be the sha2_256 hash of asset1 ++ asset2.
  trace_if_false( @"Invalid SwapDatum beacon_name", beacon_name == beacon_name_ ) &&

  // The asset1_id must be the asset1_id that this beacon policy is for.
  trace_if_false( @"Invalid SwapDatum asset1_id", asset1_id == asset1_id_ ) &&

  // The asset1_name must be the asset1_name that this beacon policy is for.
  trace_if_false( @"Invalid SwapDatum asset1_name", asset1_name == asset1_name_ ) &&

  // The asset2_id must be the asset2_id for this beacon.
  trace_if_false( @"Invalid SwapDatum asset2_id", asset2_id == asset2_id_ ) &&

  // The asset2_name must be the asset2_name for this beacon.
  trace_if_false( @"Invalid SwapDatum asset2_name", asset2_name == asset2_name_ ) &&

  // The forward_price denominator must be > 0.
  trace_if_false( 
    @"Swap forward_price denominator not > 0", 
    ratio.denominator(forward_price) > 0 
  ) &&

  // The forward_price must be > 0.
  trace_if_false(
    @"Swap forward_price not > 0", 
    ratio.compare(forward_price,ratio.zero()) == Greater 
  ) &&

  // The reverse_price denominator must be > 0.
  trace_if_false( 
    @"Swap reverse_price denominator not > 0", 
    ratio.denominator(reverse_price) > 0 
  ) &&

  // The reverse_price must be > 0.
  trace_if_false( 
    @"Swap reverse_price not > 0", 
    ratio.compare(reverse_price,ratio.zero()) == Greater 
  )
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
      // 2) The token name must be: 
      //      sha2_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )
      
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
  trading_pairs: List<(AssetConfig,AssetConfig)>,
  beacon_names: List<AssetName>,
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The beacons must go to an addres protected by the dApp validator script.
  // 2) The beacons must go to an address using a valid staking credential.
  // 3) The beacons must be stored individually at the dApp address.
  // 4) The beacons must be stored with the proper inline `SwapDatum`:
  //      a) `beacon_id` == this policy id.
  //      b) `beacon_name` == sha2_256(asset1_id ++ asset1_name ++ asset2_id ++ asset2_name)
  //      c) `asset1_id` == asset_id of asset1 for that trading pair.
  //      d) `asset1_name` == asset_name of asset1 for that trading_pair.
  //      e) `asset2_id` == asset_id asset2 for that trading_pair.
  //      f) `asset2_name` == asset_name asset2 for that trading_pair.
  //      g) `forward_price` denominator > 0
  //      h) `forward_price` > 0
  //      i) `reverse_price` denominator > 0
  //      j) `reverse_price` > 0
  // 5) The beacons must be stored with asset1 and/or asset2.

  // Create a map from beacon names to `trading_pair` (information from redeemer).
  // Preserve current ordering; there is no need to waste units sorting it.
  let xs = list.zip(beacon_names,trading_pairs) |> dict.from_list(_,fn(_,_) { Less }) 

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
          Some((AssetConfig(asset1_id,asset1_name),AssetConfig(asset2_id,asset2_name))) -> {
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

            // The beacons must be stored with asset1 and/or asset2.
            trace_if_false(
              @"Beacons must be stored with asset1 and/or asset2",
              value.quantity_of( val, asset1_id, asset1_name ) > 0 ||
              value.quantity_of( val, asset2_id, asset2_name ) > 0
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
                   || (cn == asset1_id && tn == asset1_name) // It is asset1.
                   || (cn == asset2_id && tn == asset2_name) // It is asset2.
                   || cn == value.ada_policy_id // It is ADA.
                    )
                }
              )
            ) &&

            // The beacons must be stored with the proper inline `SwapDatum`. This function has its 
            // own error messages.
            valid_swap_datum_creation(
              beacon_id,
              beacon_name,
              asset1_id,
              asset1_name,
              asset2_id,
              asset2_name,
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
  // 1) Any beacons not burned must be re-output to a dApp address with staking and with the 
  //    proper inline `SwapDatum`:
  //      a) all fields same as input datum except the prices.
  //      b) forward_price denominator > 0
  //      c) forward_price > 0
  //      d) reverse_price denominator > 0
  //      e) reverse_price > 0
  // 2) The beacons must be stored with asset1 and/or asset2.
  // 3) The beacons must be stored individually.
  // 4) No extraneous assets can be in the swap.

  // Extract out the information from the current datum.
  let SwapDatum(
    beacon_id, 
    beacon_name,
    asset1_id,
    asset1_name,
    asset2_id,
    asset2_name,
    _, // The prices aren't needed since they can be replaced.
    _, // The prices aren't needed since they can be replaced.
    _ // prev_input isn't needed since it will be reset to None.
  ) = swap_datum

  // Extract out the dapp_cred from the address.
  let Address(dapp_cred,_) = swap_addr

  // This function is used to map over the outputs to check validity.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the number of beacons in this UTxO.
    let beacon_num = value.quantity_of( val, beacon_id, beacon_name )

    // If the UTxO contains a beacon, it must be checked:
    if beacon_num > 0 {
      // Get a list of all assets in this UTxO. This will be used to check that no
      // extraneous assets are present.
      let assets_present = value.flatten(val)

      // The beacon must be stored at a dapp address with staking. Uses `app_name` in 
      // the error message.
      trace_if_false( 
        concat(@"Beacon must go to a ", app_name) |> concat(_,@" dApp address with staking"),
        pay == dapp_cred && is_some(stake)
      ) &&

      // The beacons must be stored individually.
      trace_if_false(
        @"Beacons must be stored individually",
        beacon_num == 1
      ) &&

      // The beacons must be stored with some of the offered asset.
      trace_if_false(
        @"Beacons must be stored with asset1 and/or asset2",
        value.quantity_of( val, asset1_id, asset1_name ) > 0 ||
        value.quantity_of( val, asset2_id, asset2_name ) > 0
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
             || (cn == asset1_id && tn == asset1_name) // It is asset1.
             || (cn == asset2_id && tn == asset2_name) // It is asset2.
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
        asset1_id,
        asset1_name,
        asset2_id,
        asset2_name,
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
  beacon_name: AssetName,
  asset1_id: PolicyId,
  asset1_name: AssetName,
  asset2_id: PolicyId,
  asset2_name: AssetName,
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
      if value.quantity_of( oval, beacon_id, beacon_name ) == 1 {
        // Check if the output is to this swap address.
        if swap_addr == addr {
          // Check if the datum is correct.
          let SwapDatum(
            beacon_id_,
            beacon_name_,
            asset1_id_,
            asset1_name_,
            asset2_id_,
            asset2_name_,
            forward_price_,
            reverse_price_,
            prev_input
          ) = parse_datum(d)

          // Check if the prev_input field points to this input.
          if prev_input == Some(input_ref) {
            // The beacon_id must be the policy id of the beacon policy.
            expect True = error_if_false( 
              @"Invalid SwapDatum beacon_id", 
              beacon_id == beacon_id_ 
            )

            // The beacon_name must be the beacon_name from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum beacon_name", 
              beacon_name == beacon_name_ 
            )

            // The asset1_id must be the asset1_id from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum asset1_id", 
              asset1_id == asset1_id_ 
            )

            // The asset1_name must be the asset1_name from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum asset1_name", 
              asset1_name == asset1_name_ 
            )

            // The asset2_id must be the asset2_id from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum asset2_id", 
              asset2_id == asset2_id_ 
            )

            // The asset2_name must be the asset2_name from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum asset2_name", 
              asset2_name == asset2_name_ 
            )

            // The forward_price must be the forward_price from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum forward_price", 
              forward_price == forward_price_
            )

            // The reverse_price must be the reverse_price from the input.
            expect True = error_if_false( 
              @"Invalid SwapDatum reverse_price", 
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
              beacon_name,
              asset1_id,
              asset1_name,
              asset2_id,
              asset2_name,
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
            beacon_name,
            asset1_id,
            asset1_name,
            asset2_id,
            asset2_name,
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
          beacon_name,
          asset1_id,
          asset1_name,
          asset2_id,
          asset2_name,
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
  beacon_name: AssetName,
  asset1_id: PolicyId,
  asset1_name: AssetName,
  asset2_id: PolicyId,
  asset2_name: AssetName,
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
    beacon_name,
    asset1_id,
    asset1_name,
    asset2_id,
    asset2_name,
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
