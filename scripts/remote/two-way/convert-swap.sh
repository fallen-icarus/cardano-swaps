#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

ownerPubKeyFile="$HOME/wallets/01Stake.vkey"
swapAddrFile="${tmpDir}twoWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
swapRedeemerFile="${tmpDir}twoWaySpendingRedeemer.json"
beaconRedeemerFile="${tmpDir}twoWayBeaconRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
beaconScriptPreprodTestnetRef="115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#1"
beaconScriptSize=4707

spendingScriptPreprodTestnetRef="115c9ebb9928b8ec6e0c9d1420c43421cfb323639dd9fdcf1e7155e73bec13c5#0"
spendingScriptSize=5343

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --update-with-mint \
  --out-file $swapRedeemerFile

# Create the swap datum.
echo "Creating the swap datum..."
cardano-swaps datums two-way \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --first-price '1 / 1000000' \
  --second-price '3000000 / 1' \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
  --stdout)

oldPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

oldAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldAsset1Beacon="${beaconPolicyId}.${oldAsset1BeaconName}"
oldAsset2Beacon="${beaconPolicyId}.${oldAsset2BeaconName}"

newPairBeaconName=$(cardano-swaps beacon-info two-way pair-beacon \
  --first-asset lovelace \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newAsset1BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --first-asset lovelace \
  --stdout)

newAsset2BeaconName=$(cardano-swaps beacon-info two-way asset-beacon \
  --second-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newAsset1Beacon="${beaconPolicyId}.${newAsset1BeaconName}"
newAsset2Beacon="${beaconPolicyId}.${newAsset2BeaconName}"

# Create the minting redeemer. This redeemer can also burn the old beacons.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((336576154))

echo "Building the initial transaction..."
cardano-cli conway transaction build-raw \
  --tx-in ac20e6b1cb9723b736f5360253ba3628d17051b7d20a5abb6b83e45b43065128#1 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#1 \
  --tx-in 84e649f06340eb887663dd5211259367b27638f842778b14382359983130bfb2#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
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
spend_0_mem=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="spend" and .validator.index==0) | .budget.memory' )
spend_0_steps=$(echo $exec_units | jq '.result | .[] | select(.validator.purpose=="spend" and .validator.index==0) | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli conway transaction build-raw \
  --tx-in ac20e6b1cb9723b736f5360253ba3628d17051b7d20a5abb6b83e45b43065128#1 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#1 \
  --tx-in 84e649f06340eb887663dd5211259367b27638f842778b14382359983130bfb2#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 5000000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli conway transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --reference-script-size $((beaconScriptSize+spendingScriptSize)) \
  --witness-count 2 | cut -d' ' -f1)
req_fee=$((calculated_fee+100000)) # Add 0.1 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli conway transaction build-raw \
  --tx-in ac20e6b1cb9723b736f5360253ba3628d17051b7d20a5abb6b83e45b43065128#1 \
  --tx-in f6538bb7a6a0365f3c295aed389df29e752df504ebe691e34f45c0ab9f96272c#1 \
  --tx-in 84e649f06340eb887663dd5211259367b27638f842778b14382359983130bfb2#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon} + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --mint "-1 ${oldPairBeacon} + -1 ${oldAsset1Beacon} + -1 ${oldAsset2Beacon} + 1 ${newPairBeacon} + 1 ${newAsset1Beacon} + 1 ${newAsset2Beacon}" \
  --mint-tx-in-reference $beaconScriptPreprodTestnetRef \
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
cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file $HOME/wallets/01.skey \
  --signing-key-file $HOME/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"

# Add a newline after the submission response.
echo ""
