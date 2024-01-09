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
  --tx-in aec62de69f36fc9860d0d4bcf2ff8836dd32e5d27a43169b2df35efc4a16b263#1 \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
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
