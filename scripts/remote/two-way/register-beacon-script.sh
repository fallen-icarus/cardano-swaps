#!/bin/sh

# WARNING
# It is no longer possible to register the beacon scripts as written since they do not allow
# certificate executions. Prior to the conway error, scripts did not need to be executed to register
# them. Now, they do. The beacon scripts for the current cardano-swaps version were registered prior
# to the conway era. They cannot be de-registered.
#
# This template script is for explanatory purposes only based off the new conway era rules.

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

beaconScriptFile="${tmpDir}twoWayBeacons.plutus"
beaconRedeemer="${tmpDir}registerTwoWayBeacons.plutus"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
beaconScriptPreprodTestnetRef="115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#1"
beaconScriptSize=4707

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts two-way beacon-script \
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
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-reference-tx-in-execution-units "(0,0)" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

# MAKE SURE THE INDEXES MATCH THE LEXICOGRAPHICAL ORDERING FOR INPUTS AND POLICY IDS.
# You can use `cardano-cli debug transaction view --tx-file "${tmpDir}tx.body` to view the prior
# transaction with everything in the correct order.
cert_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="certificate" and .validator.index==0) | .budget.memory' )
cert_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="certificate" and .validator.index==0) | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli conway transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + $initial_change lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-tx-in-reference $beaconScriptPreprodTestnetRef \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-reference-tx-in-execution-units "(${cert_steps},${cert_mem})" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --reference-script-size $((beaconScriptSize)) \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$((calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Building the final transaction..."
cardano-cli conway transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-tx-in-reference $beaconScriptPreprodTestnetRef \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --certificate-reference-tx-in-execution-units "(${cert_steps},${cert_mem})" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
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
