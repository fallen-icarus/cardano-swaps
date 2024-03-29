#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

swapAddr1="addr_test1zqql5djxthlrdcnvy87m7uswf0d0es9cdw6nvl72gcqj743ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqv7yg2s"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"

# Create the Swap redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --swap \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --offer-price '1000000 / 1' \
  --input-swap-ref 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#0 \
  --out-file $swapDatumFile1

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId1=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName1=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

offerBeaconName1=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

askBeaconName1=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon1="${beaconPolicyId1}.${pairBeaconName1}"
offerBeacon1="${beaconPolicyId1}.${offerBeaconName1}"
askBeacon1="${beaconPolicyId1}.${askBeaconName1}"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((8976772040-5000000-3000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df#1 \
  --tx-in 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#0 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "${swapAddr1} + 8000000 lovelace + 1 ${pairBeacon1} + 1 ${offerBeacon1} + 1 ${askBeacon1} + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
  --tx-in 07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df#1 \
  --tx-in 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#0 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "${swapAddr1} + 8000000 lovelace + 1 ${pairBeacon1} + 1 ${offerBeacon1} + 1 ${askBeacon1} + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
  --tx-out-count 3 \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$((calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df#1 \
  --tx-in 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#0 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-out "$(cat ../../../ignored/wallets/02.addr) + 3000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "${swapAddr1} + 8000000 lovelace + 1 ${pairBeacon1} + 1 ${offerBeacon1} + 1 ${askBeacon1} + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
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
