#!/bin/sh

# A helper script for showing how to store the universal swap validator on chain. Beacon
# policies can be stored on chain in the same transaction.

## Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

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
cardano-cli transaction build \
  --tx-in 28bd750b45a4459f6b2d184e6ed504a4ba54a8b8b21c9cf0eaa42ed12b5ab004#0 \
  --tx-in 0f94a13cf0207e9a322c15d52d372dc04bd292160277f94e4fc5fbad5598a209#0 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 26000000 lovelace " \
  --tx-out-reference-script-file $swapScriptFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 17000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"