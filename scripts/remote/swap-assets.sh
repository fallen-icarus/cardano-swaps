#!/bin/sh

# Variables
dir="../../ignored/swap-files/"
tmpDir="../../ignored/tmp/"

swapAddr1="addr_test1zp6s8lsapktf3f9wjm299er2vvk7glp4len08crxjrrdykeualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq5wlj7k"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}swap.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps swap-redeemer \
  --swap \
  --out-file $swapRedeemerFile

# Create the new swap datum. The CLI can calculate the weighted avg price for you.
echo "Creating the new swap datum..."
cardano-swaps swap-datum \
  --offer-lovelace \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --utxo-balance 10000000 \
  --price-numerator 1 \
  --price-denominator 50000 \
  --out-file $swapDatumFile1

# Helper beacon variables.
beaconPolicyId1=$(cardano-swaps beacon-info policy-id \
  --offer-lovelace \
  --stdout)

beaconName1=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --stdout)

beacon1="${beaconPolicyId1}.${beaconName1}"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps export-protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((7572534776+2000000+3000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in e7a3bad0208f66ba5f3be5607a75d153e2827d748cd7644d3e2fd2d91b5f3b02#1 \
  --tx-in eb9c6b9dbce88d8e1c92e481353d39ee3f15874c2c5b8c79e1930e8afc843418#0 \
  --tx-in e7a3bad0208f66ba5f3be5607a75d153e2827d748cd7644d3e2fd2d91b5f3b02#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --tx-out "$swapAddr1 + 8000000 lovelace + 1 ${beacon1} + 40 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace + 110 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(curl -s -L -X POST 'https://cardano-preprod.blockfrost.io/api/v0/utils/txs/evaluate' \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/cbor' \
  -H "project_id: $(cat ../../ignored/api.txt)" \
  --data $(jq .cborHex "${tmpDir}tx.body" | tr -d '"'))

spend_0_mem=$(echo $exec_units | jq '.result.EvaluationResult."spend:0".memory')
spend_0_steps=$(echo $exec_units | jq '.result.EvaluationResult."spend:0".steps')

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in e7a3bad0208f66ba5f3be5607a75d153e2827d748cd7644d3e2fd2d91b5f3b02#1 \
  --tx-in eb9c6b9dbce88d8e1c92e481353d39ee3f15874c2c5b8c79e1930e8afc843418#0 \
  --tx-in e7a3bad0208f66ba5f3be5607a75d153e2827d748cd7644d3e2fd2d91b5f3b02#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$swapAddr1 + 8000000 lovelace + 1 ${beacon1} + 40 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace + 110 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 3 \
  --tx-out-count 2 \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in e7a3bad0208f66ba5f3be5607a75d153e2827d748cd7644d3e2fd2d91b5f3b02#1 \
  --tx-in eb9c6b9dbce88d8e1c92e481353d39ee3f15874c2c5b8c79e1930e8afc843418#0 \
  --tx-in e7a3bad0208f66ba5f3be5607a75d153e2827d748cd7644d3e2fd2d91b5f3b02#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$swapAddr1 + 8000000 lovelace + 1 ${beacon1} + 40 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace + 110 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --protocol-params-file "${tmpDir}protocol.json" \
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
