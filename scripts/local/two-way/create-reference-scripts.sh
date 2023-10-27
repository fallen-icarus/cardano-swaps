#!/bin/sh

# A helper script for showing how to store the reference scripts on chain.

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}twoWaySwap.plutus"
beaconPolicyFile="${dir}twoWayBeacons.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-swaps scripts two-way beacon-policy \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Building the transaction..."
cardano-cli transaction build \
  --tx-in fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --tx-in fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
  --tx-in fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#2 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 31000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 19000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
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
