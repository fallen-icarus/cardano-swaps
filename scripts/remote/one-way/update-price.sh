#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

beaconScriptFile="${dir}oneWayBeacons.plutus"
beaconAddrFile="${dir}oneWayBeaconStake.addr"

swapAddrFile="${dir}oneWaySwap.addr"

swapDatumFile="${dir}swapDatum.json"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"
beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Export the beacon script.
echo "Exporting the beacon script..."
cardano-swaps scripts one-way beacon-script \
  --out-file $beaconScriptFile

# Create the meta beacon stake address.
echo "Creating the beacon reward address..."
cardano-cli stake-address build \
  --stake-script-file $beaconScriptFile \
  --testnet-magic 1 \
  --out-file $beaconAddrFile

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --update-with-stake \
  --out-file $swapRedeemerFile

# Create the beacon script redeemer.
echo "Creating the beacon redeemer..."
cardano-swaps beacon-redeemers one-way \
  --update-only \
  --out-file $beaconRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --offer-price 2000000 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

askBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"
askBeacon="${beaconPolicyId}.${askBeaconName}"

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((316605149))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 5befc3f68d6ff1f8f7842bf27163a415cefd7f3dfca1c1f47b7668937307cca1#1 \
  --tx-in a0253efa191f62d2eae47230e9deacfa16923faf6d13074d460f57a934f2ca0e#0 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#1 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-execution-units "(0,0)" \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
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
spend_0_mem=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="spend:1") | .budget.memory' )
spend_0_steps=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="spend:1") | .budget.cpu' )
stake_0_mem=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="withdrawal:0") | .budget.memory' )
stake_0_steps=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="withdrawal:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in 5befc3f68d6ff1f8f7842bf27163a415cefd7f3dfca1c1f47b7668937307cca1#1 \
  --tx-in a0253efa191f62d2eae47230e9deacfa16923faf6d13074d460f57a934f2ca0e#0 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#1 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-execution-units "(${stake_0_steps},${stake_0_mem})" \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
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
  --tx-in 5befc3f68d6ff1f8f7842bf27163a415cefd7f3dfca1c1f47b7668937307cca1#1 \
  --tx-in a0253efa191f62d2eae47230e9deacfa16923faf6d13074d460f57a934f2ca0e#0 \
  --spending-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --withdrawal "$(cat ${beaconAddrFile})+0" \
  --withdrawal-tx-in-reference 1ccd7e32ac5b978d6eb3b62b8243a78e192ce56e234892087d567dc33797fc5d#1 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-execution-units "(${stake_0_steps},${stake_0_mem})" \
  --withdrawal-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
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
