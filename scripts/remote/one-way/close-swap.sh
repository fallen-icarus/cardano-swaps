#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"

beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --close \
  --out-file $swapRedeemerFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"

# Creating the beacon policy redeemer. 
echo "Creating the beacon policy redeemer..."
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((8180604059))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#2 \
  --tx-in 8064545d5c06fcd051eedf4f2d5a2e6efdc08b376720f21b1b3457d48bb536e1#1 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 8000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon} + -1 ${askBeacon}" \
  --mint-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
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
mint_mem=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#2 \
  --tx-in 8064545d5c06fcd051eedf4f2d5a2e6efdc08b376720f21b1b3457d48bb536e1#1 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 8000000 lovelace + 5 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon} + -1 ${askBeacon}" \
  --mint-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
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
  --witness-count 2 | cut -d' ' -f1)
req_fee=$((calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 841f95b65531a8bfe076336a544b62466057848e039ea31e519e2c852add4090#2 \
  --tx-in 8064545d5c06fcd051eedf4f2d5a2e6efdc08b376720f21b1b3457d48bb536e1#1 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 8000000 lovelace + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon} + -1 ${askBeacon}" \
  --mint-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../../ignored/wallets/01.skey \
  --signing-key-file ../../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
