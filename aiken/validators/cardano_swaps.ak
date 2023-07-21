use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/transaction/credential.{Script}
use aiken/transaction/value

use cardano_swaps/types.{ 
  AssetConfig,
  BeaconRedeemer,
  BurnBeacons,
  CloseOrUpdate,
  MintBeacons,
  Swap,
  SwapDatum,
  SwapRedeemer
}
use cardano_swaps/utils.{trace_if_false}

validator() {
  fn spend(datum: SwapDatum, redeemer: SwapRedeemer, ctx: ScriptContext) -> Bool {
    // Get required information for script context.
    let ScriptContext( 
      Transaction(inputs,_,outputs,_,_,_,wtdr,_,sigs,..), 
      tx.Spend(out_ref)
    ) = ctx
    expect Some(Input(_, Output(addr,val,..))) = tx.find_input(inputs,out_ref)

    when redeemer is {
      // Close or update open swap positions.
      CloseOrUpdate -> {
        // Requirements:
        // 1) Staking credential must signal approval.
        // 2) Any beacons not burned must be re-output to this address with the proper inline SwapDatum.
        //      all fields same as input datum except the price
        //      price denominator > 0
        //      price > 0
        // 3) The beacons must be stored with some of the offered asset.
        // 4) No extraneous assets can be stored in the swap UTxO.
        // 5) The beacons must be stored individually.
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all trading pairs must get their own swap UTxOs.
        //
        // Requirement 5 forces the user to burn extra beacons. Extra beacons can arise when
        // swap inputs are consolidated during an asset swap.
        
        // The address' staking credential must signal approval (Requirement 1).
        trace_if_false( 
          @"Staking credential did not approve", 
          utils.staking_credential_approves( addr, wtdr, sigs)
        ) &&

        // The outputs must be valid. This checks requirements 2,3,4. It has its own error messages.
        utils.valid_update_outputs( datum, outputs, addr )
      }

      // Swap assets. This uses all inputs with the same beacon and from the same address to get the
      // value flux. This allows the same address to be used for different swaps within the same tx.
      Swap -> {
        // Requirements:
        // 1) The input must contain the beacon for that trading pair.
        // 2) All beacons must be re-output to this address with the proper inline SwapDatum.
        //      all fields same as input datum except the price
        //      price must be weighted avg of all relevant input prices
        // 3) Offered asset taken * weighted avg price <= asked asset given
        // 4) Only the offered asset leaves.
        //
        // Requirement 1 guarantees that all invalid UTxOs belong to the address owner and that
        // swap inputs have a valid price:
        //      denominator > 0
        //      price > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all trading pairs must get their own swap UTxOs.

        // Extract out the information from this datum.
        let SwapDatum(
          beacon_id, 
          beacon_name,
          offer_id,
          offer_name,
          ask_id,
          ask_name,
          _ // Not needed since presence of beacons means price is valid.
        ) = datum

        // The input must have the swap beacon for this trading pair.
        trace_if_false( 
          @"Swap input missing swap beacon",
          value.quantity_of(val,beacon_id,beacon_name) > 0
        ) &&

        // The following checks requirements 2,3,4. It has its own error messages.
        utils.valid_swap(
          beacon_id,
          beacon_name,
          offer_id,
          offer_name,
          ask_id,
          ask_name,
          outputs,
          inputs,
          addr
        )
      }
    }
  }
}

validator(offer_config: AssetConfig, dapp_hash: Hash<Blake2b_224, Script>) {
  fn mint(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    // Get required information from the script context.
    let ScriptContext( Transaction(_,_,outputs,_,mint,..), tx.Mint(policy_id) ) = ctx

    when redeemer is {
      // Mint a beacon for those trading pairs.
      MintBeacons(ask_assets) -> {
        // Requirements:
        // 1) Only the beacons for the target assets can/must be minted (they must appear in the
        //      redeemer).
        // 2) The token name for the beacons must be:
        //      sha2_256( ask_asset policy id ++ ask_asset asset name )
        // 3) The beacons must go to an addres protected by the dApp validator script.
        // 4) The beacons must go to an address using a valid staking credential.
        // 5) The beacons must be stored individually at the dApp address.
        // 6) The beacons must be stored with the proper inline SwapDatum:
        //      beacon_id == this policy id.
        //      beacon_name == sha2_256( ask_asset policy id ++ ask_asset asset name )
        //      offer_id == asset_id of offer_config
        //      offer_name == asset_name of offer_config
        //      ask_id == asset_id of ask_asset
        //      ask_name == asset_name of ask_asset
        //      price denominator > 0.
        //      price > 0
        // 7) The beacons must be stored with some of the offered asset.
        // 8) No extraneous assets can be stored in the swap UTxO.

        let beacon_names = list.map(ask_assets,utils.generate_beacon_name)
        let AssetConfig( offer_id, offer_name ) = offer_config

        // This function checks requirements 1 & 2.
        utils.mint_check( policy_id, beacon_names, redeemer, mint ) &&
        
        // This function checks the rest of the requirements.
        utils.beacon_destination_check(
          dapp_hash,
          policy_id,
          offer_id,
          offer_name,
          beacon_names,
          ask_assets,
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