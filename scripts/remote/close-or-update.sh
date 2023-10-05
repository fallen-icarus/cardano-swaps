#!/bin/sh

# Any beacons not re-output to the swap address must be burned.

# The following example closes one swap and updates the asking price for another swap.

# Variables
dir="../../ignored/swap-files/"
tmpDir="../../ignored/tmp/"

ownerPubKeyFile="../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}swap.addr"

swapDatumFile1="${dir}swapDatum1.json"

swapRedeemerFile="${dir}closeOrUpdate.json"

beaconRedeemerFile1="${dir}beaconRedeemer1.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the CloseOrUpdate redeemer.
echo "Creating the spending redeemer..."
cardano-swaps swap-redeemer \
  --close-or-update \
  --out-file $swapRedeemerFile

# Create the new swap datum.
echo "Creating the new swap datum..."
cardano-swaps swap-datum \
  --offer-lovelace \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --price-numerator 20 \
  --price-denominator 1000000 \
  --out-file $swapDatumFile1

# Helper beacon variables.
beaconPolicyId1=$(cardano-swaps beacon-info policy-id \
  --offer-lovelace \
  --stdout)

beaconName1=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 4f74686572546f6b656e0a \
  --stdout)

beaconName2=$(cardano-swaps beacon-info asset-name \
  --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --ask-token-name 54657374546f6b656e34 \
  --stdout)

beacon1="${beaconPolicyId1}.${beaconName1}"
beacon2="${beaconPolicyId1}.${beaconName2}"

# Creating the beacon policy redeemer. If no beacons need to be minted, it is cheaper to use
# the BurnBeacons redeemer. 
echo "Creating the beacon policy redeemer..."
cardano-swaps beacon-redeemer burn \
  --out-file $beaconRedeemerFile1

# However, if even one beacon must be minted by that policy, the CreateSwap is required instead. In 
# this scenario, the beacons actually being minted/burned must be included in the redeemer. All 
# beacon policies must get their own redeemer. 
#
# cardano-swaps beacon-redeemer mint \
#   --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --ask-token-name 4f74686572546f6b656e0a \
#   --ask-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
#   --ask-token-name 54657374546f6b656e34 \
#   --out-file $beaconRedeemerFile1

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps export-protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((7562795553+10000000))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#2 \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#1 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "-1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile1 \
  --mint-reference-tx-in-execution-units "(0,0)" \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee 0 \
  --out-file "${tmpDir}tx.body"

echo "Getting the execution units estimations..."
exec_units=$(curl -s -L -X POST 'https://cardano-preprod.blockfrost.io/api/v0/utils/txs/evaluate' \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/cbor' \
  -H "project_id: $(cat ../../ignored/api.txt)" \
  --data $(jq .cborHex "${tmpDir}tx.body" | tr -d '"'))

spend_0_mem=$(echo $exec_units | jq '.result.EvaluationResult."spend:0".memory')
spend_0_steps=$(echo $exec_units | jq '.result.EvaluationResult."spend:0".steps')
spend_1_mem=$(echo $exec_units | jq '.result.EvaluationResult."spend:1".memory')
spend_1_steps=$(echo $exec_units | jq '.result.EvaluationResult."spend:1".steps')
mint_mem=$(echo $exec_units | jq '.result.EvaluationResult."mint:0".memory')
mint_steps=$(echo $exec_units | jq '.result.EvaluationResult."mint:0".steps')

echo "Rebuilding the transaction with proper execution budgets..."
cardano-cli transaction build-raw \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#2 \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#1 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_1_steps},${spend_1_mem})" \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + ${initial_change} lovelace " \
  --mint "-1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile1 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --policy-id "$beaconPolicyId1" \
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
  --tx-out-count 2 \
  --witness-count 2 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#2 \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#0 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --tx-in ce60dee5e44ebc154e949382e01464c8d9119eb5f414ade38ca7080e43b1b76b#1 \
  --spending-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --spending-reference-tx-in-execution-units "(${spend_1_steps},${spend_1_mem})" \
  --tx-out "$(cat ${swapAddrFile}) + 10000000 lovelace + 1 ${beacon1}" \
  --tx-out-inline-datum-file $swapDatumFile1 \
  --tx-out "$(cat ../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "-1 ${beacon2}" \
  --mint-tx-in-reference 6ea0a8eb9d0ad061c816b1207c21dedddf3d3c1b438d5541fefbf200b87ba705#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile1 \
  --mint-reference-tx-in-execution-units "(${mint_steps},${mint_mem})" \
  --policy-id "$beaconPolicyId1" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --tx-total-collateral 21000000 \
  --required-signer-hash "$ownerPubKeyHash" \
  --protocol-params-file "${tmpDir}protocol.json" \
  --fee $req_fee \
  --out-file "${tmpDir}tx.body"

echo "Signing the transaction..."
cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../../ignored/wallets/01.skey \
  --signing-key-file ../../ignored/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

echo "Submitting the transaction..."
cardano-swaps submit \
  --testnet \
  --tx-file "${tmpDir}tx.signed"
