#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}oneWaySwap.plutus" # This is used to create the swap address.

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}oneWaySwap.addr"

swapDatumFile="${dir}swapDatum.json"

beaconRedeemerFile="${dir}createSwap.json"

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts one-way swap-script \
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
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"

# Get the mint beacon redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers one-way \
  --create-swap \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --price-numerator 1000000 \
  --price-denominator 1 \
  --out-file $swapDatumFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((57588660-3000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 6def0fb5c759ef91cf0120616a5012a33adfedaa82f67d6b83279ad1d0ebda56#2 \
  --tx-in 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#2 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 2000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon}" \
  --mint-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli transaction build-raw \
  --tx-in 6def0fb5c759ef91cf0120616a5012a33adfedaa82f67d6b83279ad1d0ebda56#2 \
  --tx-in 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#2 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 2000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon}" \
  --mint-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
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
  --tx-in-count 2 \
  --tx-out-count 3 \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with proper transaction fee..."
cardano-cli transaction build-raw \
  --tx-in 6def0fb5c759ef91cf0120616a5012a33adfedaa82f67d6b83279ad1d0ebda56#2 \
  --tx-in 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#2 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 2000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon}" \
  --mint-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --fee $req_fee \
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
