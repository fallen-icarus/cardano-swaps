#!/bin/sh

# Variables
dir="../../ignored/swap-files/"
tmpDir="../../ignored/tmp/"

swapScriptFile="${dir}spend.plutus" # This is used to create the swap address.

ownerPubKeyFile="../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}swap.addr"

swapDatumFile1="${dir}swapDatum1.json"
swapDatumFile2="${dir}swapDatum2.json"

beaconRedeemerFile="${dir}createSwap.json"

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
echo "Calculating the beacon names..."
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
echo "Exporting the current protocol parameters..."
cardano-swaps export-protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((7583059234-20000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 6db024cde5401d4a8e58e10170f543da29a8534208277b83355dd538519ea550#2 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon2}" \
  --tx-out-inline-datum-file $swapDatumFile2 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${beacon1} + 1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(curl -s -L -X POST 'https://cardano-preprod.blockfrost.io/api/v0/utils/txs/evaluate' \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/cbor' \
  -H "project_id: $(cat ../../ignored/api.txt)" \
  --data $(jq .cborHex "${tmpDir}tx.body" | tr -d '"'))

mint_mem=$(echo $exec_units | jq '.result.EvaluationResult."mint:0".memory')
mint_steps=$(echo $exec_units | jq '.result.EvaluationResult."mint:0".steps')

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli transaction build-raw \
  --tx-in 6db024cde5401d4a8e58e10170f543da29a8534208277b83355dd538519ea550#2 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon2}" \
  --tx-out-inline-datum-file $swapDatumFile2 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${beacon1} + 1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 1 \
  --tx-out-count 3 \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 6db024cde5401d4a8e58e10170f543da29a8534208277b83355dd538519ea550#2 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon2}" \
  --tx-out-inline-datum-file $swapDatumFile2 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "1 ${beacon1} + 1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --fee $req_fee \
  --protocol-params-file "${tmpDir}protocol.json" \
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
