#!/bin/sh

# A helper script for showing how to store reference scripts on-chain without having to rely
# on a local node.

## Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}twoWaySwap.plutus"
beaconPolicyFile="${dir}twoWayBeacon.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
  --out-file $swapScriptFile

## Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts two-way beacon-script \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((8180352038))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#0 \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#1 \
  --tx-in 44f58115ad9738de64bb5624b495a2e7abddfdbe055df474d7d980af1244d64e#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 24000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 22000000 lovelace " \
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
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#0 \
  --tx-in 38fd18f4ca7c6587eb2703ac3bfd42e1406d089901e2c29f158358fdda5b196a#1 \
  --tx-in 44f58115ad9738de64bb5624b495a2e7abddfdbe055df474d7d980af1244d64e#1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 24000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 22000000 lovelace " \
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
