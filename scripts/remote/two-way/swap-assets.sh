#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapAddr1="addr_test1zzseu072la47qt0mct984lqjmp37sn5gyv4x2vam42gum6pualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqqgram2"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}twoWaySpendingRedeemer.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --forward-price-numerator 1000000 \
  --forward-price-denominator 1 \
  --reverse-price-numerator 1 \
  --reverse-price-denominator 1000000 \
  --tx-hash d60f1f75bbfc267e47a04282da77821f99d343f8555afe18e70d151733c3ab36 \
  --output-index 0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info two-way pair-beacon \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --stdout)

asset1BeaconName1=$(cardano-swaps beacon-info two-way asset-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName1=$(cardano-swaps beacon-info two-way asset-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId1}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId1}.${asset2BeaconName1}"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((10582364-2000000-3000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in bd4fc68a26e112f6481ed242f04cbc2d24b4313f80804753873e297e44c84a86#1 \
  --tx-in d60f1f75bbfc267e47a04282da77821f99d343f8555afe18e70d151733c3ab36#0 \
  --spending-tx-in-reference 892c8e0c1037403c4a92f6abc991a8e21e0d97e9af68d3a36d8aabe7067ac67a#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 4 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$swapAddr1 + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
spend_0_mem=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="spend:1") | .budget.memory' )
spend_0_steps=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="spend:1") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in bd4fc68a26e112f6481ed242f04cbc2d24b4313f80804753873e297e44c84a86#1 \
  --tx-in d60f1f75bbfc267e47a04282da77821f99d343f8555afe18e70d151733c3ab36#0 \
  --spending-tx-in-reference 892c8e0c1037403c4a92f6abc991a8e21e0d97e9af68d3a36d8aabe7067ac67a#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 4 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$swapAddr1 + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
  --tx-in bd4fc68a26e112f6481ed242f04cbc2d24b4313f80804753873e297e44c84a86#1 \
  --tx-in d60f1f75bbfc267e47a04282da77821f99d343f8555afe18e70d151733c3ab36#0 \
  --spending-tx-in-reference 892c8e0c1037403c4a92f6abc991a8e21e0d97e9af68d3a36d8aabe7067ac67a#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 4 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$swapAddr1 + 5000000 lovelace + 1 ${pairBeacon1} + 1 ${asset1Beacon1} + 1 ${asset2Beacon1} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
