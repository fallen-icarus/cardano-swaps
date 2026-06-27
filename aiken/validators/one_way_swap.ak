use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction,Input,Output,OutputReference} as tx
use cardano/address.{Script,Credential}
use cardano/certificate.{Certificate,RegisterCredential}
use cardano/assets.{PolicyId}

use cardano_swaps/one_way_swap/types.{ 
  SpendWithMint,
  SpendWithStake,
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
        let Transaction{inputs,outputs,validity_range,..} = tx
        expect Some(Input{output: Output{address: addr, value: val, ..}, ..}) = tx.find_input(inputs,input_ref)

        // Extract out the information from this datum.
        expect Some(SwapDatum{
          beacon_id, 
          pair_beacon,
          offer_id,
          offer_name,
          offer_beacon,
          ask_id,
          ask_name,
          ask_beacon,
          swap_price,
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
          offer_id,
          offer_name,
          offer_beacon,
          ask_id,
          ask_name,
          ask_beacon,
          swap_price,
          expiration,
          outputs
        )
      }
    }
  }
}

// The logic for `CreateOrCloseSwaps` and `UpdateSwaps` are identical except for the fact that
// `CreateOrCloseSwaps` can mint/burn beacons due to being executed as a minting script. The ledger
// will prevent `UpdateSwaps` from minting since staking script executions cannot mint/burn tokens.
validator beacon_script(dapp_hash: ScriptHash) {
  publish(_redeemer, cert: Certificate, _tx: Transaction) -> Bool {
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
    //      k) `expiration` >= invalid-hereafter && `expiration` % 60000 == 0 if expiration used
    // 6) The offer asset and the ask asset must be different assets.
    // 7) Any unused beacons must be burned.
    //
    // To enable real-time front-ends, the expirations are set to 1-min intervals. This way
    // front-ends will not struggle to keep up with the on-chain order book.

    // Get required information from the script context.
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
    //      k) `expiration` >= invalid-hereafter && `expiration` % 60000 == 0 if expiration used
    // 6) The offer asset and the ask asset must be different assets.
    // 7) Any unused beacons must be burned.
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
