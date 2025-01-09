#!/bin/sh

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

beaconScriptFile="${dir}twoWayBeacons.plutus"

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts two-way beacon-script \
  --out-file $beaconScriptFile

# Create the registration certificate
cardano-cli stake-address registration-certificate \
  --stake-script-file $beaconScriptFile \
  --out-file "${tmpDir}registration.cert"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((59627618-2000000)) # registration requires 2 ADA deposit.

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $initial_change lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
req_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 2 \
  --tx-out-count 2 \
  --witness-count 1 | cut -d' ' -f1)

echo "Building the final transaction..."
cardano-cli transaction build-raw \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $((initial_change-req_fee)) lovelace" \
  --certificate-file "${tmpDir}registration.cert" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
