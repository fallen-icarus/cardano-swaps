use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/transaction/credential.{Script}
use aiken/transaction/value

use cardano_swaps/two_way_swap/types.{ 
  BeaconRedeemer,
  BurnBeacons,
  CloseOrUpdate,
  CreateSwap,
  ForwardSwap,
  ReverseSwap,
  SwapDatum,
  SwapRedeemer
}
use cardano_swaps/common/utils.{trace_if_false} as common_utils
use cardano_swaps/two_way_swap/utils as utils

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
        // 1) Staking credential must signal approval.
        // 2) Any beacons not burned must be re-output to this address with the proper inline 
        //    SwapDatum:
        //      a) all fields same as input datum except the prices.
        //      b) forward_price denominator > 0
        //      c) forward_price > 0
        //      d) reverse_price denominator > 0
        //      e) reverse_price > 0
        // 3) The beacons must be stored with asset1 and/or asset2.
        // 4) No extraneous assets can be stored in the swap UTxO.
        // 5) The beacons must be stored individually.
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 5 forces the user to burn extra beacons. 

        // The address' staking credential must signal approval (Requirement 1).
        trace_if_false( 
          @"Staking credential did not approve", 
          common_utils.staking_credential_approves( addr, wtdr, sigs)
        ) &&

        // The outputs must be valid. This checks requirements 2,3,4. It has its own error messages.
        utils.valid_update_outputs( datum, outputs, addr )
      }

      // Swap asset1 for asset2. It compares the input value against the corresponding output
      // value to determine the value flux. Asset2 is the offered asset and Asset1 is the ask asset.
      ForwardSwap -> {
        // Requirements:
        // 1) The input must contain the beacon for that trading pair.
        // 2) There must must be an output to this address with the proper value and inline
        //    SwapDatum:
        //      a) Must contain exactly 1 beacon.
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        // 3) Asset2 taken * price <= Asset1 given
        // 4) Only Asset2 leaves and only Asset1 is deposited. ADA can always be deposited.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      a) forward_price denominator > 0
        //      b) `forward_price` > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 4 helps optimize beacon queries by keeping UTxO asset lists to the smallest
        // possible size.

        // Extract out the information from this datum.
        let SwapDatum(
          beacon_id, 
          beacon_name,
          asset1_id,
          asset1_name,
          asset2_id,
          asset2_name,
          forward_price,
          reverse_price,
          _ // prev_input is not needed.
        ) = datum

        // The input must have the swap beacon for this trading pair.
        trace_if_false( 
          @"Swap input missing swap beacon",
          value.quantity_of(val,beacon_id,beacon_name) > 0
        ) &&

        // The following checks requirements 2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          addr, // This address.
          beacon_id,
          beacon_name,
          asset1_id,
          asset1_name,
          asset2_id,
          asset2_name,
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
        // 1) The input must contain the beacon for that trading pair.
        // 2) There must must be an output to this address with the proper value and inline
        //    SwapDatum:
        //      a) Must contain exactly 1 beacon.
        //      b) The swap datum must be exactly the same as the input's except the prev_input
        //         must be == Some(input_ref).
        // 3) Asset1 taken * price <= Asset2 given
        // 4) Only Asset1 leaves and only Asset2 is deposited. ADA can always be deposited.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      a) reverse_price denominator > 0
        //      b) `reverse_price` > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 4 helps optimize beacon queries by keeping UTxO asset lists to the smallest
        // possible size.

        // Extract out the information from this datum.
        let SwapDatum(
          beacon_id, 
          beacon_name,
          asset1_id,
          asset1_name,
          asset2_id,
          asset2_name,
          forward_price,
          reverse_price,
          _ // prev_input is not needed.
        ) = datum

        // The input must have the swap beacon for this trading pair.
        trace_if_false( 
          @"Swap input missing swap beacon",
          value.quantity_of(val,beacon_id,beacon_name) > 0
        ) &&

        // The following checks requirements 2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          addr, // This address.
          beacon_id,
          beacon_name,
          asset1_id,
          asset1_name,
          asset2_id,
          asset2_name,
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
  fn mint(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    // Get required information from the script context.
    expect ScriptContext( Transaction(_,_,outputs,_,mint,..), tx.Mint(policy_id) ) = ctx

    when redeemer is {
      // Mint a beacon for that trading pairs.
      CreateSwap(trading_pairs) -> {
        // Requirements:
        // 1) The first asset in the sorted trading pair is asset1 and the second asset is
        //    asset2.
        // 2) Beacons can only be minted with this redeemer.
        // 3) The token name for the beacons must be:
        //      sha_256( asset1_id ++ asset1_name ++ asset2_id ++ asset2_name )
        // 3) The beacons must go to an address protected by the dApp validator script.
        // 4) The beacons must go to an address using a valid staking credential.
        // 5) The beacons must be stored individually at the dApp address.
        // 6) The beacons must be stored with the proper inline SwapDatum:
        //      a) beacon_id == this policy id.
        //      c) asset1_id == asset_id of asset1
        //      d) asset1_name == asset_name of asset1
        //      e) asset2_id == asset_id of asset2
        //      f) asset2_name == asset_name of asset2
        //      g) forward_price denominator > 0.
        //      h) forward_price > 0
        //      i) reverse_price denominator > 0.
        //      j) reverse_price > 0
        //      k) prev_input == None.
        // 7) The beacons must be stored with asset1 and/or asset2.
        // 8) No extraneous assets can be stored in the swap UTxO.

        // Sort the trading pairs. This is to ensure beacon tokens are independent of the trading
        // pair direction.
        let sorted_trading_pairs = list.map(trading_pairs,utils.sort_pair)
        let beacon_names = list.map(sorted_trading_pairs,utils.generate_beacon_name) 

        // This function checks requirements 1 & 2.
        utils.mint_check( policy_id, beacon_names, redeemer, mint ) &&

        // This function checks the rest of the requirements.
        utils.beacon_destination_check(
          dapp_hash,
          policy_id,
          sorted_trading_pairs,
          beacon_names,
          outputs
        )
      }

      // Burn beacons.
      BurnBeacons -> {
        // Always allowed as long as this redeemer is only used to burn.
        utils.mint_check( policy_id, [], redeemer, mint )
      }
    }
  }
}
