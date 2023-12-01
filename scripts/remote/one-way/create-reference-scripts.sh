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

## Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=57451469

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in b1575cd53b47fd4265dbb09c3ffb43ee07e9092bb25757b707dd9fa06badd7ef#0 \
  --tx-in 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#0 \
  --tx-in 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 26000000 lovelace" \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 18000000 lovelace" \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
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
  --tx-in b1575cd53b47fd4265dbb09c3ffb43ee07e9092bb25757b707dd9fa06badd7ef#0 \
  --tx-in 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#0 \
  --tx-in 98471060e651cc6e60b863f2bb37bdefbc64d8faa17513aa2974f0beec8430d6#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 18000000 lovelace " \
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
