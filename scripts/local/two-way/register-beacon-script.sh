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
cardano-cli transaction build \
  --tx-in e39e1414f0ba51220be1e1a11b8379a3ef629ebb6bca8d4e11ad11076c762263#1 \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --change-address "$(cat ../../../ignored/wallets/01.addr)" \
  --certificate-file "${tmpDir}registration.cert" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
