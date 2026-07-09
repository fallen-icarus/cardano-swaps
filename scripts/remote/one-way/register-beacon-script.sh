#!/bin/sh

# A helper script for showing how to register the scripts for staking executions.
# The scripts may already be registered! Once registered, they cannot be delegated or
# de-registered.

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

beaconScriptFile="${tmpDir}oneWayBeacons.plutus"
beaconRedeemer="${tmpDir}registerOneWayBeacons.plutus"

# The reference scripts may already be locked on-chain. Check the one-way swap address without a
# staking credential. Both the spending script and the beacon script will be permanently locked in
# this address.
#
# cardano-cli conway address build \
#   --payment-script-file $swapScriptFile \
#   --testnet-magic 1 \
#   --out-file $swapAddrFile
#
# You can use the `cardano-swaps query personal-address` command to see them.

beaconScriptPreprodTestnetRef="b1d92732ba5392ba76129360bb838f80c0177a71f757dcec58e3f15b8aa1b3fe#1"
beaconScriptSize=4614

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconScriptFile

# Create the registration certificate
cardano-cli conway stake-address registration-certificate \
  --stake-script-file $beaconScriptFile \
  --key-reg-deposit-amt 2000000 \
  --out-file "${tmpDir}registration.cert"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((59627618-2000000)) # registration requires 2 ADA deposit.

echo "Building the initial transaction..."
cardano-cli conway transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + $initial_change lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-tx-in-reference $beaconScriptPreprodTestnetRef \
  --certificate-plutus-script-v3 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-reference-tx-in-execution-units "(0,0)" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) 21000000 lovelace" \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

# MAKE SURE THE INDEXES MATCH THE LEXICOGRAPHICAL ORDERING FOR INPUTS AND POLICY IDS.
# You can use `cardano-cli debug transaction view --tx-file "${tmpDir}tx.body` to view the prior
# transaction with everything in the correct order.
cert_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="publish" and .validator.index==0) | .budget.memory' )
cert_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="publish" and .validator.index==0) | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli conway transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + $initial_change lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-tx-in-reference $beaconScriptPreprodTestnetRef \
  --certificate-plutus-script-v3 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-reference-tx-in-execution-units "(${cert_steps},${cert_mem})" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) 21000000 lovelace" \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --reference-script-size $((beaconScriptSize)) \
  --witness-count 1 --output-json | jq .fee)
req_fee=$((calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.
req_collateral=$(printf %.0f $(echo "${req_fee}*1.5" | bc))

echo "Building the final transaction..."
cardano-cli conway transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-tx-in-reference $beaconScriptPreprodTestnetRef \
  --certificate-plutus-script-v3 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-reference-tx-in-execution-units "(${cert_steps},${cert_mem})" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral $req_collateral \
  --tx-out-return-collateral "$(cat $HOME/wallets/01.addr) $((21000000-$req_collateral)) lovelace" \
  --fee $req_fee \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
