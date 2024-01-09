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
  --tx-in 892c8e0c1037403c4a92f6abc991a8e21e0d97e9af68d3a36d8aabe7067ac67a#0 \
  --tx-in 892c8e0c1037403c4a92f6abc991a8e21e0d97e9af68d3a36d8aabe7067ac67a#1 \
  --tx-in 9ab26f84b2bab49473216b774b873cf4c6fdeabc8b5780d4d7c4409e522727ff#0 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 24000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 22000000 lovelace " \
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
