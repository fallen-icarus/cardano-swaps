use aiken/hash.{Hash,Blake2b_224}
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/transaction/credential.{Script,ScriptCredential,Inline}

use cardano_swaps/one_way_swap/types.{ 
  BeaconRedeemer,
  CreateOrCloseSwaps,
  SpendWithMint,
  SpendWithStake,
  UpdateSwaps,
  Swap,
  SwapDatum,
  SwapRedeemer
}
use cardano_swaps/common/utils.{trace_if_false} as common_utils
use cardano_swaps/one_way_swap/utils as utils

// The logic of `SpendWithMint` and `SpendWithStake` are identical except for how it expects the
// beacon_script to be executed: `SpendWithMint` expects a minting policy execution and
// `SpendWithStake` expects a staking script execution. The reason for this is that there are
// scenarios where swaps need to be updated but no beacons need to be minted/burned. Only one type
// of execution is ever needed in the transaction (you can also update swaps using `SpendWithMint`).
// The idea is that `SpendWithMint` is meant to pair with `CreateOrCloseSwaps` (this allows closing
// swaps and changing the swap's trading pair), and `SpendWithStake` is meant to pair with
// `UpdateSwaps` (this allows cheaply updating swaps when no minting/burning is needed).
validator() {
  fn swap_script(datum: SwapDatum, redeemer: SwapRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      SpendWithMint -> {
        // 1) The address' staking credential must signal approval.
        // 2) The beacon script must be executed as a minting policy.

        // Get required information for script context.
        expect ScriptContext( 
          Transaction(inputs,_,_,_,mint,_,wtdr,_,sigs,..), 
          tx.Spend(input_ref)
        ) = ctx
        expect Some(Input(_, Output(addr,..))) = tx.find_input(inputs,input_ref)
        let SwapDatum(beacon_id,..) = datum

        // The address' staking credential must signal approval (Requirement 1).
        trace_if_false( 
          @"Staking credential did not approve", 
          common_utils.staking_credential_approves( addr, wtdr, sigs)
        ) &&
        
        // The beacon_script was not executed as a minting policy.
        trace_if_false(
          @"Beacon script not executed as minting policy",
          common_utils.has_beacon_script_minting_execution(beacon_id,mint)
        )
      }

      SpendWithStake -> {
        // 1) The address' staking credential must signal approval.
        // 2) The beacon script must be executed as a staking script.

        // Get required information for script context.
        expect ScriptContext( 
          Transaction(inputs,_,_,_,_,_,wtdr,_,sigs,..), 
          tx.Spend(input_ref)
        ) = ctx
        expect Some(Input(_, Output(addr,..))) = tx.find_input(inputs,input_ref)
        let SwapDatum(beacon_id,..) = datum

        // The address' staking credential must signal approval (Requirement 1).
        trace_if_false( 
          @"Staking credential did not approve", 
          common_utils.staking_credential_approves( addr, wtdr, sigs)
        ) &&
        
        // The beacon_script was not executed as a staking script.
        trace_if_false(
          @"Beacon script not executed as staking script",
          common_utils.has_beacon_script_staking_execution(beacon_id,wtdr)
        )
      }

      Swap -> {
        // Requirements:
        // 1) The input must contain the beacons for that trading pair.
        // 2) There must must be an output to this address with the proper value and inline
        //    SwapDatum:
        //      a) Must contain exactly 1 of each of the required beacons.
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        // 3) Offer taken * price <= Ask given
        // 4) Only the offer asset leaves and only the ask asset is deposited. Ada can always be 
        //    deposited.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      a) `swap_price` denominator > 0
        //      b) `swap_price` numerator > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 4 helps optimize beacon queries by keeping UTxO asset lists to the smallest
        // possible size.

        // Get required information for script context.
        expect ScriptContext( 
          Transaction(inputs,_,outputs,..), 
          tx.Spend(input_ref)
        ) = ctx
        expect Some(Input(_, Output(addr,val,..))) = tx.find_input(inputs,input_ref)

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

        // The following checks requirements 1,2,3,4. It has its own error messages.
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

// The logic for `CreateOrCloseSwaps` and `UpdateSwaps` are identical except for the fact that
// `CreateOrCloseSwaps` can mint/burn beacons due to being executed as a minting script. The ledger
// will prevent `UpdateSwaps` from minting since staking script executions cannot mint/burn tokens.
validator(dapp_hash: Hash<Blake2b_224, Script>) {
  fn beacon_script(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      CreateOrCloseSwaps -> {
        // Requirements:
        // 1) Must be executed as a minting script.
        // 2) The beacons must go to an address protected by the DApp validator script.
        // 3) The beacons must go to an address using a valid staking credential.
        // 4) The UTxOs with the beacons must have the proper value:
        //      a) Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
        //      b) The beacons must correspond to the beacons in the datum.
        //      c) There must be exactly 1 of each beacon.
        //      d) No extraneous assets are in the UTxO.
        // 5) The beacons must be stored with the proper inline `SwapDatum`:
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
        // 6) The offer asset and the ask asset must be different assets.
        // 7) Any unused beacons must be burned.

        // Get required information from the script context.
        let ScriptContext( Transaction(_,_,outputs,..), purpose) = ctx

        let policy_id = when purpose is {
          tx.Mint(policy_id) -> policy_id
          _ -> error @"Redeemer not used with minting execution"
        }

        utils.beacon_destination_check(
          dapp_hash,
          policy_id,
          outputs
        )
      }

      UpdateSwaps -> {
        // Requirements:
        // 1) Must be executed as a staking script.
        // 2) The beacons must go to an address protected by the DApp validator script.
        // 3) The beacons must go to an address using a valid staking credential.
        // 4) The UTxOs with the beacons must have the proper value:
        //      a) Exactly three kinds of beacons: pair beacon, offer beacon, and ask beacon.
        //      b) The beacons must correspond to the beacons in the datum.
        //      c) There must be exactly 1 of each beacon.
        //      d) No extraneous assets are in the UTxO.
        // 5) The beacons must be stored with the proper inline `SwapDatum`:
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
        // 6) The offer asset and the ask asset must be different assets.

        // Get required information from the script context.
        let ScriptContext( Transaction(_,_,outputs,..), purpose) = ctx

        let policy_id = when purpose is {
          tx.WithdrawFrom(Inline(ScriptCredential(policy_id))) -> policy_id
          _ -> error @"Redeemer not used with staking execution"
        }

        utils.beacon_destination_check(
          dapp_hash,
          policy_id,
          outputs
        )
      }
    }
  }
}
