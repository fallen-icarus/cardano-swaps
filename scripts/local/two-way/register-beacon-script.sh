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
# beaconScriptSize=4707

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
cardano-cli conway transaction build \
  --tx-in aec62de69f36fc9860d0d4bcf2ff8836dd32e5d27a43169b2df35efc4a16b263#1 \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --change-address "$(cat $HOME/wallets/01.addr)" \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-tx-in-reference $beaconScriptPreprodTestnetRef \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $beaconRedeemer \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
