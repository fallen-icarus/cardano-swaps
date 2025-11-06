#!/bin/sh

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

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapScriptFile="${tmpDir}oneWaySwap.plutus"
beaconScriptFile="${tmpDir}oneWayBeacons.plutus"

## Export the swap script.
echo "Exporting the swap script..."
cardano-swaps scripts one-way swap-script \
  --out-file $swapScriptFile

## Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconScriptFile

## Create and submit the transaction.
echo "Building the transaction..."
cardano-cli conway transaction build \
  --tx-in d40c0194ed824eb61eda38fabbc3d3afd08f3877617e57e02435347f7749fac1#0 \
  --tx-in d40c0194ed824eb61eda38fabbc3d3afd08f3877617e57e02435347f7749fac1#1 \
  --tx-in 283ccf650ce7dbfa6062fcfaa1df3d875dfceca64512e9fa1329abea64a845c8#0 \
  --tx-out "$(cat $HOME/wallets/01.addr) + 22000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 21000000 lovelace " \
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
