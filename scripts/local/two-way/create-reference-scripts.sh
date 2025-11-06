#!/bin/sh

# The reference scripts may already be locked on-chain. Check the two-way swap address without a
# staking credential. Both the spending script and the beacon script will be permanently locked in
# this address.
#
# cardano-cli conway address build \
#   --payment-script-file $swapScriptFile \
#   --testnet-magic 1 \
#   --out-file $swapAddrFile
#
# You can use the `cardano-swaps query personal-address` command to see them.

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapScriptFile="${tmpDir}twoWaySwap.plutus"
beaconScriptFile="${tmpDir}twoWayBeacons.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

## Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts two-way beacon-script \
  --out-file $beaconScriptFile

## Create and submit the transaction.
echo "Building the transaction..."
cardano-cli conway transaction build \
  --tx-in 2bfcae792bfef8505cf32751c00d77b9f970f44ab2258221378dcf2a7c3acd9d#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + 24000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 22000000 lovelace " \
  --tx-out-reference-script-file $beaconScriptFile \
  --change-address "$(cat $HOME/wallets/01.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
