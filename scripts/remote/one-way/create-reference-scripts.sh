#!/bin/sh

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}oneWaySwap.plutus"
beaconPolicyFile="${dir}oneWayBeacons.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts one-way swap-script \
  --out-file $swapScriptFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-swaps scripts one-way beacon-policy \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=58105897

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in d52bf2621cd5ed984e529685e3a1088db81e4fa431769bec77f3374421b20fd7#0 \
  --tx-in d52bf2621cd5ed984e529685e3a1088db81e4fa431769bec77f3374421b20fd7#1 \
  --tx-in dfc9b1a97010efae5f0489ab8485ae4fe1482f438769c44b7460d15fcb9d110b#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 18000000 lovelace " \
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
  --tx-in d52bf2621cd5ed984e529685e3a1088db81e4fa431769bec77f3374421b20fd7#0 \
  --tx-in d52bf2621cd5ed984e529685e3a1088db81e4fa431769bec77f3374421b20fd7#1 \
  --tx-in dfc9b1a97010efae5f0489ab8485ae4fe1482f438769c44b7460d15fcb9d110b#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 18000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --fee $req_fee \
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
