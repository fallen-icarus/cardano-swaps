use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction,Input,Output,OutputReference} as tx
use cardano/address.{Script,Credential}
use cardano/certificate.{Certificate,RegisterCredential}
use cardano/assets.{PolicyId}

use cardano_swaps/two_way_swap/types.{ 
  SpendWithMint,
  SpendWithStake,
  TakeAsset1,
  TakeAsset2,
  SwapDatum,
  SwapRedeemer
}
use cardano_swaps/common/utils.{trace_if_false} as common_utils
use cardano_swaps/two_way_swap/utils as utils

validator swap_script {
  spend(datum: Option<SwapDatum>, redeemer: SwapRedeemer, input_ref: OutputReference, tx: Transaction) -> Bool {
    when redeemer is {
      SpendWithMint -> {
        // 1) The address' staking credential must signal approval.
        // 2) The beacon script must be executed as a minting policy.

        // Get required information for script context.
        let Transaction{inputs: inputs, mint: mint, withdrawals: wtdr, extra_signatories: sigs, ..} = tx
        expect Some(Input{output: Output {address: addr, ..}, ..}) = tx.find_input(inputs,input_ref)
        expect Some(SwapDatum{beacon_id,..}) = datum

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
        let Transaction{inputs: inputs,withdrawals: wtdr,extra_signatories: sigs,..} = tx
        expect Some(Input{output: Output{address: addr,..}, ..}) = tx.find_input(inputs,input_ref)
        expect Some(SwapDatum{beacon_id,..}) = datum

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

      // Take asset1 and deposit asset2. It compares the input value against the corresponding output
      // value to determine the value flux.
      TakeAsset1 -> {
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
        //      a) `asset1_price` denominator > 0
        //      b) `asset1_price` numerator > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 4 helps optimize beacon queries by keeping UTxO asset lists to the smallest
        // possible size.

        // Get required information for script context.
        let Transaction{inputs,outputs,validity_range,..} = tx
        expect Some(Input{output: Output{address: addr, value: val, ..}, ..}) = tx.find_input(inputs,input_ref)

        // Extract out the information from this datum.
        expect Some(SwapDatum{
          beacon_id, 
          pair_beacon,
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          asset1_price,
          asset2_price,
          expiration,
          ..
        }) = datum

        // The following checks requirements 1,2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          utils.get_upper_bound(validity_range),
          addr, // This address.
          beacon_id,
          pair_beacon,
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          asset1_price,
          asset2_price,
          expiration,
          outputs,
          False // Not taking asset2.
        )
      }

      // Take asset2 and deposit asset1. It compares the input value against the corresponding output
      // value to determine the value flux. 
      TakeAsset2 -> {
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
        //      a) `asset2_price` denominator > 0
        //      b) `asset2_price` numerator > 0
        //
        // Requirement 2 guarantees that beacons from other trading pairs cannot be combined into
        // one output UTxO; all swaps must get their own swap UTxOs.
        //
        // Requirement 4 helps optimize beacon queries by keeping UTxO asset lists to the smallest
        // possible size.

        // Get required information for script context.
        let Transaction{inputs,outputs,validity_range,..}= tx
        expect Some(Input{output: Output{address: addr, value: val, ..}, ..}) = tx.find_input(inputs,input_ref)

        // Extract out the information from this datum.
        expect Some(SwapDatum{
          beacon_id, 
          pair_beacon,
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          asset1_price,
          asset2_price,
          expiration,
          ..
        }) = datum

        // The following checks requirements 1,2,3,4. It has its own error messages.
        utils.valid_swap(
          input_ref,
          val, // Input's value.
          utils.get_upper_bound(validity_range),
          addr, // This address.
          beacon_id,
          pair_beacon,
          asset1_id,
          asset1_name,
          asset1_beacon,
          asset2_id,
          asset2_name,
          asset2_beacon,
          asset1_price,
          asset2_price,
          expiration,
          outputs,
          True // Taking asset2.
        )
      }
    }
  }
}

validator beacon_script(dapp_hash: ScriptHash) {
  publish(_redeemer, cert: Certificate, _tx) -> Bool {
    // Only allow registering the script. It cannot be delegated or deregistered.
    // The staking execution allows updating order UTxOs in place (without changing the beacon
    // tokens).
    when cert is {
      RegisterCredential{..} -> True
      _ -> error "Publish can only be used to register the beacon script"
    }
  }

  // Used to create or close swaps
  mint(_redeemer, policy_id: PolicyId, tx: Transaction) -> Bool {
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
    //      i) `asset1_price` denominator > 0
    //      j) `asset1_price` numerator > 0
    //      k) `asset2_price` denominator > 0
    //      l) `asset2_price` numerator > 0
    //      m) asset1 < asset2
    //      n) `expiration` >= invalid-hereafter && `expiration` % 60000 == 0 if expiration used
    //
    // The validator assumes the trading pairs are sorted which is why asset1 must be less than
    // asset2. Asset1 and asset2 cannot be the same asset.
    //
    // To enable real-time front-ends, the expirations are set to 1-min intervals. This way
    // front-ends will not struggle to keep up with the on-chain order book.

    // Get required information from the Transaction.
    let Transaction{outputs,validity_range,..} = tx

    utils.beacon_destination_check(
      dapp_hash,
      policy_id,
      utils.get_upper_bound(validity_range),
      outputs
    )
  }

  // Used to update swaps
  withdraw(_redeemer, account: Credential, tx: Transaction) -> Bool {
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
    //      i) `asset1_price` denominator > 0
    //      j) `asset1_price` numerator > 0
    //      k) `asset2_price` denominator > 0
    //      l) `asset2_price` numerator > 0
    //      m) asset1 < asset2
    //      n) `expiration` >= invalid-hereafter && `expiration` % 60000 == 0 if expiration used
    //
    // The validator assumes the trading pairs are sorted which is why asset1 must be less than
    // asset2. Asset1 and asset2 cannot be the same asset.
    //
    // To enable real-time front-ends, the expirations are set to 1-min intervals. This way
    // front-ends will not struggle to keep up with the on-chain order book.

    // Get required information from the script context.
    let Transaction{outputs,validity_range,..} = tx

    let policy_id = when account is {
      Script(policy_id) -> policy_id
      _ -> error @"Withdraw must be from a script"
    }

    utils.beacon_destination_check(
      dapp_hash,
      policy_id,
      utils.get_upper_bound(validity_range),
      outputs
    )
  }
}
