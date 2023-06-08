#!/bin/sh

# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

spendingScriptFile="${dir}spend.plutus"
beaconPolicyFile="${dir}beacon.plutus"

swapAddrFile="${dir}swap.addr"

swapBeaconDatumFile="${dir}beaconDatum.json"
swapPositionDatumFile="${dir}positionDatum.json"

beaconRedeemerFile="${dir}mint.json"

# Export the spending script for that trading pair.
cardano-swaps export-script swap-script \
  --offered-asset-is-lovelace \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $spendingScriptFile

# Export the beacon policy for that trading pair.
cardano-swaps export-script beacon-policy \
  --offered-asset-is-lovelace \
  --asked-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asked-asset-token-name 4f74686572546f6b656e0a \
  --out-file $beaconPolicyFile

# Create the swap address.
cardano-cli address build \
  --payment-script-file $spendingScriptFile \
  --stake-verification-key-file "../assets/wallets/01Stake.vkey" \
  --testnet-magic 1 \
  --out-file $swapAddrFile

# Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

# Helper beacon variable
beacon="${beaconPolicyId}."

# Create the datum for storing with the beacon
cardano-swaps datum beacon-datum \
  --beacon-policy-id $beaconPolicyId \
  --out-file $swapBeaconDatumFile

# Create the datum for the first swap positions
cardano-swaps datum swap-datum \
  --price-numerator 1 \
  --price-denominator 1000000 \
  --out-file $swapPositionDatumFile

# Create the beacon redeemer for minting the beacon.
cardano-swaps beacon-redeemer \
  --mint \
  --out-file $beaconRedeemerFile

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in a4c18c0c51a466553b97f0ed60ef6c7bc3181fe2ceec1555fc72137d3357a344#0 \
  --tx-out "$(cat ${swapAddrFile}) + 23000000 lovelace + 1 ${beacon}" \
  --tx-out-inline-datum-file $swapBeaconDatumFile \
  --tx-out-reference-script-file $spendingScriptFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace" \
  --tx-out-inline-datum-file $swapPositionDatumFile \
  --mint "1 ${beacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"