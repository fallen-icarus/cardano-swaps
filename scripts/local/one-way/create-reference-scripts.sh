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
# Preprod: addr_test1wqql5djxthlrdcnvy87m7uswf0d0es9cdw6nvl72gcqj74s38ksy4
# Mainnet: addr1wyql5djxthlrdcnvy87m7uswf0d0es9cdw6nvl72gcqj74s20zvts

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
  --tx-in edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#0 \
  --tx-in edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#1 \
  --tx-in d4fbd706c39717ff0391fcb8dec5c643e9156bae98bb2a07cbf60948b05c28db#1 \
  --tx-out "$(cat $HOME/wallets/01.addr) + 22000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 20000000 lovelace " \
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

# Add a newline after the submission response.
echo ""
