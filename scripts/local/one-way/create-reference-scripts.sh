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
echo "Building the transaction..."
cardano-cli transaction build \
  --tx-in 5ae04fb39873e137cfbffcdecedbad4908d8665d53994c8aa56837893be9341d#0 \
  --tx-in 5ae04fb39873e137cfbffcdecedbad4908d8665d53994c8aa56837893be9341d#1 \
  --tx-in 5739e05188623dd36933b5dfdccddcc975ea3964f7a0bd2265a283879530cb49#0 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 18000000 lovelace " \
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
