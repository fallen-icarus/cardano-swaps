#!/bin/sh

# A helper script for showing how to store the universal swap validator on chain. Beacon
# policies can be stored on chain in the same transaction.

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
echo "Building the transaction..."
cardano-cli transaction build \
  --tx-in 67873119e94effdfd9f3af6fdfef5f6a9d90b71e26cca80f3a17b5ce097eeb46#0 \
  --tx-in 67873119e94effdfd9f3af6fdfef5f6a9d90b71e26cca80f3a17b5ce097eeb46#1 \
  --tx-in 67873119e94effdfd9f3af6fdfef5f6a9d90b71e26cca80f3a17b5ce097eeb46#2 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + 18000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --change-address "$(cat ../../ignored/wallets/01.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../ignored/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
