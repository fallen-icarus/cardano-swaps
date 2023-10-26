#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapScriptFile="${dir}spend.plutus" # This is used to create the swap address.

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}twoWaySwap.addr"

swapDatumFile1="${dir}swapDatum1.json"

beaconRedeemerFile="${dir}createSwap.json"

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts two-way swap-script \
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
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset-lovelace \
  --second-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --second-token-name 4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --first-asset-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --second-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --second-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Get the mint beacon redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --create-swap \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --first-asset-lovelace \
  --second-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --second-token-name 4f74686572546f6b656e0a \
  --forward-price-numerator 1000000 \
  --forward-price-denominator 1 \
  --reverse-price-numerator 1 \
  --reverse-price-denominator 1000000 \
  --out-file $swapDatumFile1

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((11706041+2000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in de4b75b57cb756f237274e120492aeb89d6a0703c78544a3add64dc8d261e474#0 \
  --tx-in de4b75b57cb756f237274e120492aeb89d6a0703c78544a3add64dc8d261e474#1 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
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
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli transaction build-raw \
  --tx-in de4b75b57cb756f237274e120492aeb89d6a0703c78544a3add64dc8d261e474#0 \
  --tx-in de4b75b57cb756f237274e120492aeb89d6a0703c78544a3add64dc8d261e474#1 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
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
  --tx-in-count 2 \
  --tx-out-count 2 \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with proper transaction fee..."
cardano-cli transaction build-raw \
  --tx-in de4b75b57cb756f237274e120492aeb89d6a0703c78544a3add64dc8d261e474#0 \
  --tx-in de4b75b57cb756f237274e120492aeb89d6a0703c78544a3add64dc8d261e474#1 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1}" \
  --mint-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId1" \
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