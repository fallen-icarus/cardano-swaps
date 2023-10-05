#!/bin/sh

# A helper script for showing how to store the universal swap validator on chain without relying
# on a local node. Beacon policies can be stored on chain in the same transaction.

## Variables
dir="../../ignored/swap-files/"
tmpDir="../../ignored/tmp/"

swapScriptFile="${dir}spend.plutus"
beaconPolicyFile="${dir}offerAdaBeacon.plutus"

## Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps export-script swap-script \
  --out-file $swapScriptFile

## Export the desired offer beacon policy.
echo "Exporting the target beacon policy script..."
cardano-swaps export-script beacon-policy \
  --offer-lovelace \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps export-protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=115221686

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in b1746f4291ac8ca4b108f62acfd0537919fc8873cb03bb450b9bc3d7dc7024ed#0 \
  --tx-in b1746f4291ac8ca4b108f62acfd0537919fc8873cb03bb450b9bc3d7dc7024ed#1 \
  --tx-in b1746f4291ac8ca4b108f62acfd0537919fc8873cb03bb450b9bc3d7dc7024ed#2 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + 18000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
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
  --tx-in b1746f4291ac8ca4b108f62acfd0537919fc8873cb03bb450b9bc3d7dc7024ed#0 \
  --tx-in b1746f4291ac8ca4b108f62acfd0537919fc8873cb03bb450b9bc3d7dc7024ed#1 \
  --tx-in b1746f4291ac8ca4b108f62acfd0537919fc8873cb03bb450b9bc3d7dc7024ed#2 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + 18000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --fee $req_fee \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
