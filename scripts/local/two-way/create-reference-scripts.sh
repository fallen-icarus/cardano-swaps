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
  --tx-in c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#0 \
  --tx-in c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#1 \
  --tx-in 652ee7042b5dec846d16082168ff88fddf0ee1f7cdd3fa94397eccbbbf835f66#2 \
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
