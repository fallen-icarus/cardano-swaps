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

initial_change=8181764766

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 8f224fb4d358bfc9c05b1784d412c2b9161d66a2246734a400a52578ceef26f5#2 \
  --tx-in 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#0 \
  --tx-in 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 22000000 lovelace" \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 20000000 lovelace" \
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
  --tx-in 8f224fb4d358bfc9c05b1784d412c2b9161d66a2246734a400a52578ceef26f5#2 \
  --tx-in 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#0 \
  --tx-in 8762f07fef0c5137ee7d6d8bce962f29554f1ddff3883f1b2d2fc39f213df94c#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 22000000 lovelace" \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 20000000 lovelace" \
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
