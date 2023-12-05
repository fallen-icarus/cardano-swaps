use aiken/hash.{Hash,Blake2b_224}
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/transaction/credential.{Script,ScriptCredential,Inline}

use cardano_swaps/two_way_swap/types.{ 
  BeaconRedeemer,
  UpdateSwaps,
  CreateOrCloseSwaps,
  SpendWithMint,
  SpendWithStake,
  ForwardSwap,
  ReverseSwap,
  SwapDatum,
  SwapRedeemer
}
use cardano_swaps/common/utils.{trace_if_false} as common_utils
use cardano_swaps/two_way_swap/utils as utils

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

      // Swap asset1 for asset2. It compares the input value against the corresponding output
      // value to determine the value flux. Asset2 is the offered asset and Asset1 is the ask asset.
      ForwardSwap -> {
        // Requirements:
        // 1) The input must contain the beacons for that trading pair.
        // 2) There must must be an output to this address with the proper value and inline
        //    SwapDatum:
        //      a) Must contain exactly 1 of each of the required beacons.
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        // 3) Asset2 taken * price <= Asset1 given
        // 4) Only Asset2 leaves and only Asset1 is deposited. Ada can always be deposited.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      a) `forward_price` denominator > 0
        //      b) `forward_price` numerator > 0
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
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          forward_price,
          reverse_price,
          _ // prev_input is not needed.
        ) = datum

        // The following checks requirements 1,2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          addr, // This address.
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
          outputs,
          False // Not reverse swap.
        )
      }

      // Swap asset2 for asset1. It compares the input value against the corresponding output
      // value to determine the value flux. Asset1 is the offered asset and Asset2 is the ask asset.
      ReverseSwap -> {
        // Requirements:
        // 1) The input must contain the beacons for that trading pair.
        // 2) There must must be an output to this address with the proper value and inline
        //    SwapDatum:
        //      a) Must contain exactly 1 of each of the required beacons.
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        // 3) Asset1 taken * price <= Asset2 given
        // 4) Only Asset1 leaves and only Asset2 is deposited. Ada can always be deposited.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      a) `reverse_price` denominator > 0
        //      b) `reverse_price` numerator > 0
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
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          forward_price,
          reverse_price,
          _ // prev_input is not needed.
        ) = datum

        // The following checks requirements 1,2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          addr, // This address.
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
          outputs,
          True // Is reverse swap.
        )
      }
    }
  }
}

validator(dapp_hash: Hash<Blake2b_224, Script>) {
  fn beacon_script(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      CreateOrCloseSwaps -> {
        // Requirements:
        // 1) Must be executed as a minting script.
        // 2) The beacons must go to an address protected by the DApp validator script.
        // 3) The beacons must go to an address using a valid staking credential.
        // 4) The UTxOs with the beacons must have the proper value:
        //      a) Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
        //      b) The beacons must correspond to the beacons in the datum.
        //      c) There must be exactly 1 of each beacon.
        //      d) No extraneous assets are in the UTxO.
        // 5) The beacons must be stored with the proper inline `SwapDatum`:
        //      a) `beacon_id` == this policy id.
        //      b) `pair_beacon` == sha2_256(asset1_id ++ asset1_name ++ asset2_id ++ asset2_name)
        //      c) `asset1_id` == policy id of asset1 for that trading pair.
        //      d) `asset1_name` == asset name of asset1 for that trading_pair.
        //      e) `asset1_beacon` == sha2_256(asset1_id ++ asset1_name).
        //      f) `asset2_id` == policy id of asset2 for that trading_pair.
        //      g) `asset2_name` == asset name of asset2 for that trading_pair.
        //      h) `asset2_beacon` == sha2_256(asset2_id ++ asset2_name).
        //      i) `forward_price` denominator > 0
        //      j) `forward_price` numerator > 0
        //      k) `reverse_price` denominator > 0
        //      l) `reverse_price` numerator > 0
        //      m) asset1 < asset2
        //
        // The validator assumes the trading pairs are sorted which is why asset1 must be less than
        // asset2. Asset1 and asset2 cannot be the same asset.

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
        //      a) Exactly three kinds of beacons: pair beacon, asset1 beacon, and asset2 beacon.
        //      b) The beacons must correspond to the beacons in the datum.
        //      c) There must be exactly 1 of each beacon.
        //      d) No extraneous assets are in the UTxO.
        // 5) The beacons must be stored with the proper inline `SwapDatum`:
        //      a) `beacon_id` == this policy id.
        //      b) `pair_beacon` == sha2_256(asset1_id ++ asset1_name ++ asset2_id ++ asset2_name)
        //      c) `asset1_id` == policy id of asset1 for that trading pair.
        //      d) `asset1_name` == asset name of asset1 for that trading_pair.
        //      e) `asset1_beacon` == sha2_256(asset1_id ++ asset1_name).
        //      f) `asset2_id` == policy id of asset2 for that trading_pair.
        //      g) `asset2_name` == asset name of asset2 for that trading_pair.
        //      h) `asset2_beacon` == sha2_256(asset2_id ++ asset2_name).
        //      i) `forward_price` denominator > 0
        //      j) `forward_price` numerator > 0
        //      k) `reverse_price` denominator > 0
        //      l) `reverse_price` numerator > 0
        //      m) asset1 < asset2
        //
        // The validator assumes the trading pairs are sorted which is why asset1 must be less than
        // asset2. Asset1 and asset2 cannot be the same asset.

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
