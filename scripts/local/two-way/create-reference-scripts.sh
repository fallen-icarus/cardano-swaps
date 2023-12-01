#!/bin/sh

# A helper script for showing how to store the reference scripts on chain.

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}twoWaySwap.plutus"
beaconScriptFile="${dir}twoWayBeacons.plutus"

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
cardano-cli transaction build \
  --tx-in 4993b083e08ac565a293140bb737b20035901c8ce8cf026d97a77eace302ada4#0 \
  --tx-in 4993b083e08ac565a293140bb737b20035901c8ce8cf026d97a77eace302ada4#1 \
  --tx-in 4e3fbfe9ada401239215fbe680c24fdba029e7c082fb1c16768389733f0eb083#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 31000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 19000000 lovelace " \
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
