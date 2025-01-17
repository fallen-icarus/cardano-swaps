#!/bin/sh

# Variables
tmpDir="/tmp/cardano-swaps/"

# Make the tmpDir if it doesn't already exist.
mkdir -p $tmpDir

ownerPubKeyFile="$HOME/wallets/01Stake.vkey"
swapAddrFile="${tmpDir}oneWaySwap.addr"
swapDatumFile="${tmpDir}swapDatum.json"
swapRedeemerFile="${tmpDir}oneWaySpendingRedeemer.json"
beaconRedeemerFile="${tmpDir}oneWayBeaconRedeemer.json"

# The reference scripts are permanently locked in the swap address without a staking credential!
# You can use the `cardano-swaps query personal-address` command to see them.
beaconScriptPreprodTestnetRef="9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#1"
beaconScriptSize=4432

spendingScriptPreprodTestnetRef="9fecc1d2cf99088facad02aeccbedb6a4f783965dc6c02bd04dc8b348e9a0858#0"
spendingScriptSize=4842

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --update-with-mint \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --offer-price 1000000 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

oldPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldOfferBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a \
  --stdout)

oldAskBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldOfferBeacon="${beaconPolicyId}.${oldOfferBeaconName}"
oldAskBeacon="${beaconPolicyId}.${oldAskBeaconName}"

newPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-asset lovelace \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newOfferBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-asset c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31 \
  --stdout)

newAskBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-asset lovelace \
  --stdout)

newPairBeacon="${beaconPolicyId}.${newPairBeaconName}"
newOfferBeacon="${beaconPolicyId}.${newOfferBeaconName}"
newAskBeacon="${beaconPolicyId}.${newAskBeaconName}"

# Creating the beacon script redeemer.
cardano-swaps beacon-redeemers one-way \
  --mint-or-burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps query protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((8180913536))

echo "Building the initial transaction..."
cardano-cli conway transaction build-raw \
  --tx-in a0253efa191f62d2eae47230e9deacfa16923faf6d13074d460f57a934f2ca0e#1 \
  --tx-in 776e817ebe6d4094d0fffba4bd175d90c8c883d5e18061dae2c3263af670c212#1 \
  --tx-in aec62de69f36fc9860d0d4bcf2ff8836dd32e5d27a43169b2df35efc4a16b263#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldOfferBeacon} + -1 ${oldAskBeacon} + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon}" \
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
  --tx-in a0253efa191f62d2eae47230e9deacfa16923faf6d13074d460f57a934f2ca0e#1 \
  --tx-in 776e817ebe6d4094d0fffba4bd175d90c8c883d5e18061dae2c3263af670c212#1 \
  --tx-in aec62de69f36fc9860d0d4bcf2ff8836dd32e5d27a43169b2df35efc4a16b263#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldOfferBeacon} + -1 ${oldAskBeacon} + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon}" \
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
req_fee=$((calculated_fee+80000)) # Add 0.08 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli conway transaction build-raw \
  --tx-in a0253efa191f62d2eae47230e9deacfa16923faf6d13074d460f57a934f2ca0e#1 \
  --tx-in 776e817ebe6d4094d0fffba4bd175d90c8c883d5e18061dae2c3263af670c212#1 \
  --tx-in aec62de69f36fc9860d0d4bcf2ff8836dd32e5d27a43169b2df35efc4a16b263#0 \
  --spending-tx-in-reference $spendingScriptPreprodTestnetRef \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon} + 8 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat $HOME/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat $HOME/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --mint "-1 ${oldPairBeacon} + -1 ${oldOfferBeacon} + -1 ${oldAskBeacon} + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon}" \
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
