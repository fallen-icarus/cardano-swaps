#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapAddr1="addr_test1zzq2x2jgmdjsgkd0xfc4xav754xmfhddyww4d05tx2jn44fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0nrmla"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}swap.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --forward-swap \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --forward-price-numerator 1000000 \
  --forward-price-denominator 1 \
  --reverse-price-numerator 1 \
  --reverse-price-denominator 1000000 \
  --tx-hash 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c \
  --output-index 0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 4f74686572546f6b656e0a \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((12239757-2000000-3000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 8d6ae7f80e56385eca80916587b9b818dfdf7505d3f31fd4ed0b5d9108c6e12c#2 \
  --tx-in 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c#0 \
  --spending-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 2 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$swapAddr1 + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + ${initial_change} lovelace" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --tx-total-collateral 21000000 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

# MAKE SURE THE INDEXES MATCH THE LEXICOGRAPHICAL ORDERING FOR INPUTS AND POLICY IDS.
spend_0_mem=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="spend:0") | .budget.memory' )
spend_0_steps=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="spend:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in 8d6ae7f80e56385eca80916587b9b818dfdf7505d3f31fd4ed0b5d9108c6e12c#2 \
  --tx-in 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c#0 \
  --spending-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 2 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$swapAddr1 + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + ${initial_change} lovelace" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --tx-total-collateral 21000000 \
  --protocol-params-file "${tmpDir}protocol.json" \
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
req_fee=$((calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 8d6ae7f80e56385eca80916587b9b818dfdf7505d3f31fd4ed0b5d9108c6e12c#2 \
  --tx-in 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c#0 \
  --spending-tx-in-reference fc587cc261a5ec23062746bec54aa1ce7ce8155b65fd133e725479f4a0072fe9#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 2 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$swapAddr1 + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 6 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + $((initial_change-req_fee)) lovelace" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --tx-total-collateral 21000000 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
