#!/bin/sh

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}oneWaySwap.plutus"
beaconScriptFile="${dir}oneWayBeacons.plutus"

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
cardano-cli transaction build \
  --tx-in edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#0 \
  --tx-in edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#1 \
  --tx-in d4fbd706c39717ff0391fcb8dec5c643e9156bae98bb2a07cbf60948b05c28db#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 22000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 20000000 lovelace " \
  --tx-out-reference-script-file $beaconScriptFile \
  --change-address "$(cat ../../../ignored/wallets/01.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
