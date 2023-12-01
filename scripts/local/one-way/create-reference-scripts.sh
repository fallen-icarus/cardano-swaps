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
  --tx-in d9607ed26f8a7a7ed9cc7fc5cfddc8cd19bba5f534fafeccc16d94516330b150#0 \
  --tx-in d9607ed26f8a7a7ed9cc7fc5cfddc8cd19bba5f534fafeccc16d94516330b150#1 \
  --tx-in ae6606058349c833ee5782db03aa24331178c39ae6a4f57a9a6ed768d8e5fe03#0 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 18000000 lovelace " \
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
