use aiken/hash.{Hash,Blake2b_224}
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/transaction/credential.{Script,ScriptCredential,Address}
use aiken/transaction/value

use cardano_swaps/one_way_swap/types.{ 
  BeaconRedeemer,
  BurnBeacons,
  CloseOrUpdate,
  CreateSwap,
  Swap,
  SwapDatum,
  SwapRedeemer
}
use cardano_swaps/common/utils.{trace_if_false} as common_utils
use cardano_swaps/one_way_swap/utils as utils

validator() {
  fn spend(datum: SwapDatum, redeemer: SwapRedeemer, ctx: ScriptContext) -> Bool {
    // Get required information for script context.
    expect ScriptContext( 
      Transaction(inputs,_,outputs,_,_,_,wtdr,_,sigs,..), 
      tx.Spend(input_ref)
    ) = ctx
    expect Some(Input(_, Output(addr,val,..))) = tx.find_input(inputs,input_ref)

    when redeemer is {
      // Close or update open swap positions.
      CloseOrUpdate -> {
        // Requirements:
        // 1) The beacons must go to an addres protected by the DApp validator script.
        // 2) The beacons must go to an address using a valid staking credential.
        // 3) The UTxOs with the beacons must have the proper value:
        //      a) Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
        //      b) The beacons must correspond to the beacons in the datum.
        //      c) There must be exactly 1 of each beacon.
        //      d) No extraneous assets are in the UTxO.
        // 4) The beacons must be stored with the proper inline `SwapDatum`:
        //      a) `beacon_id` == this policy id.
        //      b) `pair_beacon` == `sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name)`
        //      c) `offer_id` == asset id of the offer asset.
        //      d) `offer_name` == asset name of the offer asset.
        //      e) `offer_beacon` == `sha2_256("01" ++ offer_id ++ offer_name)`.
        //      f) `ask_id` == asset id of the ask asset.
        //      g) `ask_name` == asset name of the ask asset.
        //      h) `ask_beacon` == `sha2_256("02" ++ ask_id ++ ask_name)`
        //      i) `swap_price` denominator > 0
        //      j) `swap_price` > 0
        // 5) The offer asset and ask assets must be different assets.
        // 6) The address' staking credential must approve.

        expect Address(ScriptCredential(dapp_hash),_) = addr
        let SwapDatum(beacon_id,..) = datum

        // The address' staking credential must signal approval (Requirement 1).
        trace_if_false( 
          @"Staking credential did not approve", 
          common_utils.staking_credential_approves( addr, wtdr, sigs)
        ) &&

        utils.beacon_destination_check(
          dapp_hash,
          beacon_id,
          outputs
        )
      }

      Swap -> {
        // Requirements:
        // 1) The input must contain the beacon for that trading pair.
        // 2) There must must be an output to this address with the proper value and inline
        //    SwapDatum:
        //      a) Must contain exactly 1 of that trading pair beacon.
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        // 3) Offer taken * price <= Ask given
        // 4) Only the offer asset leaves and only the ask asset is deposited. Ada can always be 
        //    deposited.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      a) `swap_price` denominator > 0
        //      b) `swap_price` > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 4 helps optimize beacon queries by keeping UTxO asset lists to the smallest
        // possible size.

        // Extract out the information from this datum.
        let SwapDatum(
          beacon_id, 
          pair_beacon,
          offer_id,
          offer_name,
          offer_beacon,
          ask_id,
          ask_name,
          ask_beacon,
          swap_price,
          _ // prev_input is not needed.
        ) = datum

        // The input must have the swap beacon for this trading pair.
        trace_if_false( 
          @"Swap input missing pair beacon",
          value.quantity_of(val,beacon_id,pair_beacon) > 0
        ) &&

        // The following checks requirements 2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          addr, // This address.
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
      }
    }
  }
}

validator(dapp_hash: Hash<Blake2b_224, Script>) {
  fn mint(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    // Get required information from the script context.
    expect ScriptContext( Transaction(_,_,outputs,_,mint,..), tx.Mint(policy_id) ) = ctx

    when redeemer is {
      CreateSwap -> {
        // Requirements:
        // 1) The beacons must go to an addres protected by the DApp validator script.
        // 2) The beacons must go to an address using a valid staking credential.
        // 3) The UTxOs with the beacons must have the proper value:
        //      a) Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
        //      b) The beacons must correspond to the beacons in the datum.
        //      c) There must be exactly 1 of each beacon.
        //      d) No extraneous assets are in the UTxO.
        // 4) The beacons must be stored with the proper inline `SwapDatum`:
        //      a) `beacon_id` == this policy id.
        //      b) `pair_beacon` == `sha2_256(offer_id ++ offer_name ++ ask_id ++ ask_name)`
        //      c) `offer_id` == asset id of the offer asset.
        //      d) `offer_name` == asset name of the offer asset.
        //      e) `offer_beacon` == `sha2_256("01" ++ offer_id ++ offer_name)`.
        //      f) `ask_id` == asset id of the ask asset.
        //      g) `ask_name` == asset name of the ask asset.
        //      h) `ask_beacon` == `sha2_256("02" ++ ask_id ++ ask_name)`
        //      i) `swap_price` denominator > 0
        //      j) `swap_price` > 0
        // 5) The offer asset and the ask asset must be different assets.

        utils.beacon_destination_check(
          dapp_hash,
          policy_id,
          outputs
        )
      }

      BurnBeacons -> {
        // Always allowed as long as this redeemer is only used to burn.
        trace_if_false(
          @"Redeemer can only be used to burn",
          common_utils.beacons_only_burned(policy_id,mint)
        )
      }
    }
  }
}
