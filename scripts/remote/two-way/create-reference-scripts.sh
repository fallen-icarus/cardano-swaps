#!/bin/sh

# A helper script for showing how to store reference scripts on-chain without having to rely
# on a local node.

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}two_way_spend.plutus"
beaconPolicyFile="${dir}two_way_beacon.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-swaps scripts two-way beacon-policy \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=237707298

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in e0f0122f7cf6ca6dcb79a310886155c771bf825d057396ddb8a9ea80a18b6f7b#0 \
  --tx-in e0f0122f7cf6ca6dcb79a310886155c771bf825d057396ddb8a9ea80a18b6f7b#1 \
  --tx-in e0f0122f7cf6ca6dcb79a310886155c771bf825d057396ddb8a9ea80a18b6f7b#2 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 31000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 19000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
req_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 3 \
  --tx-out-count 3 \
  --witness-count 1 | cut -d' ' -f1)

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in e0f0122f7cf6ca6dcb79a310886155c771bf825d057396ddb8a9ea80a18b6f7b#0 \
  --tx-in e0f0122f7cf6ca6dcb79a310886155c771bf825d057396ddb8a9ea80a18b6f7b#1 \
  --tx-in e0f0122f7cf6ca6dcb79a310886155c771bf825d057396ddb8a9ea80a18b6f7b#2 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 31000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 19000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
