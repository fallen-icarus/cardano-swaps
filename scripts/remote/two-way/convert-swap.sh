#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}twoWaySwap.addr"

swapDatumFile="${dir}swapDatum.json"

swapRedeemerFile="${dir}closeOrUpdateTwoWaySwap.json"

beaconRedeemerFile="${dir}twoWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the CloseOrUpdate redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers two-way \
  --close-or-update \
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
  --out-file $swapDatumFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info two-way policy-id \
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

pairBeacon1="${beaconPolicyId}.${pairBeaconName1}"
asset1Beacon1="${beaconPolicyId}.${asset1BeaconName1}"
asset2Beacon1="${beaconPolicyId}.${asset2BeaconName1}"

pairBeaconName2=$(cardano-swaps beacon-info two-way pair-beacon \
  --asset1-is-lovelace \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --stdout)

asset1BeaconName2=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset1-is-lovelace \
  --stdout)

asset2BeaconName2=$(cardano-swaps beacon-info two-way offer-beacon \
  --asset2-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --asset2-token-name 54657374546f6b656e31 \
  --stdout)

pairBeacon2="${beaconPolicyId}.${pairBeaconName2}"
asset1Beacon2="${beaconPolicyId}.${asset1BeaconName2}"
asset2Beacon2="${beaconPolicyId}.${asset2BeaconName2}"

# Create the minting redeemer. This redeemer can also burn the old beacons.
echo "Creating the minting redeemer..."
cardano-swaps beacon-redeemers two-way \
  --create-swap \
  --out-file $beaconRedeemerFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((13447640))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c#1 \
  --tx-in d72fe434b808c3a884b5b9753cb8663363f7d1f3a7044fe8acaf167403443c24#0 \
  --tx-in 5e1b34b7a8f96345598ae563418a3a95c8191f0942bab400470b79e0a5da90dd#0 \
  --spending-tx-in-reference c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon2} + 1 ${asset1Beacon2} + 1 ${asset2Beacon2} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1} + 1 ${pairBeacon2} + 1 ${asset1Beacon2} + 1 ${asset2Beacon2}" \
  --mint-tx-in-reference c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#1 \
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

spend_0_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="spend:0") | .budget.memory' )
spend_0_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="spend:0") | .budget.cpu' )
mint_mem=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.memory' )
mint_steps=$(echo $exec_units | jq '.result | .[] | select(.validator=="mint:0") | .budget.cpu' )

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c#1 \
  --tx-in d72fe434b808c3a884b5b9753cb8663363f7d1f3a7044fe8acaf167403443c24#0 \
  --tx-in 5e1b34b7a8f96345598ae563418a3a95c8191f0942bab400470b79e0a5da90dd#0 \
  --spending-tx-in-reference c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon2} + 1 ${asset1Beacon2} + 1 ${asset2Beacon2} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1} + 1 ${pairBeacon2} + 1 ${asset1Beacon2} + 1 ${asset2Beacon2}" \
  --mint-tx-in-reference c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
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
  --tx-in-count 3 \
  --tx-out-count 3 \
  --witness-count 2 | cut -d' ' -f1)
req_fee=$(($calculated_fee+100000)) # Add 0.1 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 73ee7fcfc5fed3b5eb9159a39138cac80f609c592f4bd26e8bc4e9c34813309c#1 \
  --tx-in d72fe434b808c3a884b5b9753cb8663363f7d1f3a7044fe8acaf167403443c24#0 \
  --tx-in 5e1b34b7a8f96345598ae563418a3a95c8191f0942bab400470b79e0a5da90dd#0 \
  --spending-tx-in-reference c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ${swapAddrFile}) + 3000000 lovelace + 1 ${pairBeacon2} + 1 ${asset1Beacon2} + 1 ${asset2Beacon2} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out-inline-datum-file $swapDatumFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 11 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "-1 ${pairBeacon1} + -1 ${asset1Beacon1} + -1 ${asset2Beacon1} + 1 ${pairBeacon2} + 1 ${asset1Beacon2} + 1 ${asset2Beacon2}" \
  --mint-tx-in-reference c1d7755d9089bc1a6b85561e1f3eb740935c6a887a15589395bfc36f8b64fa10#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id "$beaconPolicyId" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee $req_fee \
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
