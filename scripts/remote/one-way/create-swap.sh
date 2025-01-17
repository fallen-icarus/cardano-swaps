#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

swapScriptFile="${tmpDir}oneWaySwap.plutus" # This is used to create the user's swap address.
ownerPubKeyFile="$HOME/wallets/01Stake.vkey"
swapAddrFile="${tmpDir}oneWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
beaconRedeemerFile="${tmpDir}oneWayBeaconRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
beaconScriptPreprodTestnetRef="9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#1"
beaconScriptSize=4432

# Export the swap validator script.
echo "Exporting the swap validator script..."
cardano-swaps scripts one-way swap-script \
  --out-file $swapScriptFile

# Create the swap address.
echo -n "Creating the swap address... "
cardano-cli conway address build \
  --payment-script-file $swapScriptFile \
  --stake-verification-key-file $ownerPubKeyFile \
  --testnet-magic 1 \
  --out-file $swapAddrFile

echo "$(cat $swapAddrFile)"

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

# Get the beacon script redeemer.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --offer-price '1000000 / 1' \
  --out-file $swapDatumFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((21607098))

echo "Building the initial transaction..."
cardano-cli conway transaction build-raw \
  --tx-in e95a73a1e03afdf74b86d10e504b64285f7afdfab7f7021a41054ae4b377ca9f#1 \
  --tx-in ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#3 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(cardano-swaps evaluate-tx \
  --testnet \
  --tx-file "${tmpDir}tx.body")

# MAKE SURE THE INDEXES MATCH THE LEXICOGRAPHICAL ORDERING FOR INPUTS AND POLICY IDS.
# You can use `cardano-cli debug transaction view --tx-file "${tmpDir}tx.body` to view the prior
# transaction with everything in the correct order.
mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="mint" and .validator.index==0) | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="mint" and .validator.index==0) | .budget.cpu' )

echo "Rebuilding the transaction with proper executions budgets..."
cardano-cli conway transaction build-raw \
  --tx-in e95a73a1e03afdf74b86d10e504b64285f7afdfab7f7021a41054ae4b377ca9f#1 \
  --tx-in ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#3 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --reference-script-size $((beaconScriptSize)) \
  --witness-count 1 | cut -d' ' -f1)
req_fee=$((calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with proper transaction fee..."
cardano-cli conway transaction build-raw \
  --tx-in e95a73a1e03afdf74b86d10e504b64285f7afdfab7f7021a41054ae4b377ca9f#1 \
  --tx-in ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#3 \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon} + 3 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --fee "$req_fee" \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
