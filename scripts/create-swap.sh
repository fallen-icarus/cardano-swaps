#!/bin/sh

# Variables
dir="../assets/swap-files/"
tmpDir="../assets/tmp/"

swapScriptFile="${dir}spend.plutus" # This is used to create the swap address.

ownerPubKeyFile="../assets/wallets/01Stake.vkey"

swapAddrFile="${dir}swap.addr"

swapDatumFile1="${dir}swapDatum1.json"
swapDatumFile2="${dir}swapDatum2.json"

beaconRedeemerFile="${dir}mintBeacons.json"

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps export-script swap-script \
  --out-file $swapScriptFile

# Create the swap address.
echo "Creating the swap address..."
cardano-cli address build \
  --payment-script-file $swapScriptFile \
  --stake-verification-key-file $ownerPubKeyFile \
  --testnet-magic 1 \
  --out-file $swapAddrFile

# Helper beacon variables.
beaconPolicyId1=$(cardano-swaps beacon-info policy-id \
  --offer-lovelace \
  --stdout)

beaconName1=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --stdout)

beaconName2=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 54657374546f6b656e34 \
  --stdout)

beacon1="${beaconPolicyId1}.${beaconName1}"
beacon2="${beaconPolicyId1}.${beaconName2}"

# Get the mint beacon redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemer mint \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 54657374546f6b656e34 \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps swap-datum \
  --offer-lovelace \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --price-numerator 10 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile1

cardano-swaps swap-datum \
  --offer-lovelace \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 54657374546f6b656e34 \
  --price-numerator 1 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile2

# Create the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 89b67297cff33ce4dfb5bbd1d2414b313cf8ee26ce0e7e970aa9c4a2682f38f5#1 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon2}" \
  --tx-out-inline-datum-file $swapDatumFile2 \
  --mint "1 ${beacon1} + 1 ${beacon2}" \
  --mint-tx-in-reference c774e01a1f0e4cd06d62780dcd52f6d00290b8217ac0e538747bf79d1a49dbfb#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
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