use aiken/hash.{Hash,Blake2b_224}
use aiken/math/rational as ratio
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/transaction/credential.{Script}

use cardano_swaps/types.{ 
  AssetConfig,
  BeaconRedeemer,
  BurnBeacons,
  CloseOrUpdate,
  MintBeacon,
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
    expect Some(Input(_, Output(addr,..))) = tx.find_input(inputs,out_ref)

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
        // 4) The beacons must be stored individually.
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all trading pairs must get their own swap UTxOs.
        //
        // Requirement 4 forces the user to burn extra beacons. Extra beacons can arise when
        // swap inputs are consolidated during an asset swap.
        
        // The address' staking credential must signal approval (Requirement 1).
        trace_if_false( 
          @"Staking credential did not approve", 
          utils.staking_credential_approves( addr, wtdr, sigs)
        ) &&

        // The outputs must be valid. This checks requirements 2,3,4.
        trace_if_false(
          @"Invalid new swap datum",
          utils.valid_update_outputs( datum, outputs, addr )
        )
      }

      // Swap assets. This uses all inputs with the same beacon and from the same address to get the
      // value flux. This allows the same address to be used for different swaps within the same tx.
      Swap -> {
        // Requirements:
        // 1) The price denominator must be > 0.
        // 2) The price must be > 0.
        // 3) All beacons must be re-output to this address with the proper inline SwapDatum.
        //      all fields same as input datum except the price
        //      price must be weighted avg of all relevant input prices
        // 4) Offered asset taken * weighted avg price <= asked asset given
        // 5) Only the offered asset leaves.
        //
        // Requirement 3 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all trading pairs must get their own swap UTxOs.

        // Extra out the information from this datum.
        let SwapDatum(
          beacon_id, 
          beacon_name,
          offer_id,
          offer_name,
          ask_id,
          ask_name,
          price
        ) = datum

        // The price denominator must be > 0.
        trace_if_false( @"Input price denominator not > 0", ratio.denominator(price) > 0 ) &&

        // The price must be > 0.
        trace_if_false( @"Input price not > 0", ratio.compare(price,ratio.zero()) == Greater ) &&

        // The following checks requirements 3,4,5. It has its own error messages.
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
      // Mint a beacon for that trading pair.
      MintBeacon(ask_asset) -> {
        // Requirements:
        // 1) Only the beacon for the target asset can/must be minted.
        // 2) The token name for the beacon must be:
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

        let AssetConfig( ask_id, ask_name ) = ask_asset
        let AssetConfig( offer_id, offer_name ) = offer_config
        let beacon_name = utils.generate_beacon_name( ask_id, ask_name )

        // This function checks requirements 1 & 2.
        utils.mint_check( policy_id, beacon_name, redeemer, mint ) &&
        
        // This function checks the rest of the requirements.
        utils.beacon_destination_check(
          dapp_hash,
          policy_id,
          beacon_name,
          offer_id,
          offer_name,
          ask_id,
          ask_name,
          outputs
        )
      }

      // Burn beacons.
      BurnBeacons -> {
        // Always allowed as long as this redeemer is only used to burn.
        utils.mint_check( policy_id, "", redeemer, mint )
      }
    }
  }
}