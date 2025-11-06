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
# beaconScriptSize=4614

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconScriptFile

echo "Exporting the redeemers..."
cardano-swaps beacon-redeemers one-way \
  --register \
  --out-file $beaconRedeemer

# Create the registration certificate
cardano-cli conway stake-address registration-certificate \
  --stake-script-file $beaconScriptFile \
  --key-reg-deposit-amt 2000000 \
  --out-file "${tmpDir}registration.cert"

# Create the transaction.
cardano-cli conway transaction build \
  --tx-in b1d92732ba5392ba76129360bb838f80c0177a71f757dcec58e3f15b8aa1b3fe#2 \
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
