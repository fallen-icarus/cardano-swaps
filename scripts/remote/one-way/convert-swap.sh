#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}oneWaySwap.addr"

swapDatumFile="${dir}swapDatum.json"

swapRedeemerFile="${dir}oneWaySpendingRedeemer.json"

beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --update-with-mint \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps datums one-way \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --price-numerator 1000000 \
  --price-denominator 1 \
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

oldPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

oldOfferBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 4f74686572546f6b656e0a \
  --stdout)

oldAskBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-lovelace \
  --stdout)

oldPairBeacon="${beaconPolicyId}.${oldPairBeaconName}"
oldOfferBeacon="${beaconPolicyId}.${oldOfferBeaconName}"
oldAskBeacon="${beaconPolicyId}.${oldAskBeaconName}"

newPairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

newOfferBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

newAskBeaconName=$(cardano-swaps beacon-info one-way ask-beacon \
  --ask-lovelace \
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
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((56421082))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in bb3a0cc58ab59db166f0c1d8ac56092a43d1e16bdd67492ba69098d5389f5a98#1 \
  --tx-in 862a296f1c2fd738075501d14d8767725aeec9cd1043811da38e119fee486a80#1 \
  --tx-in bb3a0cc58ab59db166f0c1d8ac56092a43d1e16bdd67492ba69098d5389f5a98#0 \
  --spending-tx-in-reference edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldOfferBeacon} + -1 ${oldAskBeacon} + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon}" \
  --mint-tx-in-reference edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
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
mint_mem=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo "$exec_units" | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in bb3a0cc58ab59db166f0c1d8ac56092a43d1e16bdd67492ba69098d5389f5a98#1 \
  --tx-in 862a296f1c2fd738075501d14d8767725aeec9cd1043811da38e119fee486a80#1 \
  --tx-in bb3a0cc58ab59db166f0c1d8ac56092a43d1e16bdd67492ba69098d5389f5a98#0 \
  --spending-tx-in-reference edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${oldPairBeacon} + -1 ${oldOfferBeacon} + -1 ${oldAskBeacon} + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon}" \
  --mint-tx-in-reference edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 4000 \
  --out-file "${tmpDir}tx.body"

echo "Calculating the required fee..."
calculated_fee=$(cardano-cli transaction calculate-min-fee \
  --tx-body-file "${tmpDir}tx.body" \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --tx-in-count 3 \
  --tx-out-count 3 \
  --witness-count 2 | cut -d' ' -f1)
req_fee=$((calculated_fee+80000)) # Add 0.08 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in bb3a0cc58ab59db166f0c1d8ac56092a43d1e16bdd67492ba69098d5389f5a98#1 \
  --tx-in 862a296f1c2fd738075501d14d8767725aeec9cd1043811da38e119fee486a80#1 \
  --tx-in bb3a0cc58ab59db166f0c1d8ac56092a43d1e16bdd67492ba69098d5389f5a98#0 \
  --spending-tx-in-reference edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $((initial_change-req_fee)) lovelace " \
  --mint "-1 ${oldPairBeacon} + -1 ${oldOfferBeacon} + -1 ${oldAskBeacon} + 1 ${newPairBeacon} + 1 ${newOfferBeacon} + 1 ${newAskBeacon}" \
  --mint-tx-in-reference edadc501e0da8129a9b6168be85cf4bcafffb79ef5545633028531752949c106#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
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
