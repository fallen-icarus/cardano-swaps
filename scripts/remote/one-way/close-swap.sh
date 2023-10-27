#!/bin/sh

# Variables
dir="../../../ignored/swap-files/"
tmpDir="../../../ignored/tmp/"

ownerPubKeyFile="../../../ignored/wallets/01Stake.vkey"

swapAddrFile="${dir}oneWaySwap.addr"

swapRedeemerFile="${dir}closeOrUpdateOneWaySwap.json"

beaconRedeemerFile="${dir}oneWayBeaconRedeemer.json"

# Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerPubKeyFile)

# Create the CloseOrUpdate redeemer.
echo "Creating the spending redeemer..."
cardano-swaps spending-redeemers one-way \
  --close-or-update \
  --out-file $swapRedeemerFile

# Helper beacon variables.
echo "Calculating the beacon names..."
beaconPolicyId=$(cardano-swaps beacon-info one-way policy-id \
  --stdout)

pairBeaconName=$(cardano-swaps beacon-info one-way pair-beacon \
  --ask-lovelace \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

offerBeaconName=$(cardano-swaps beacon-info one-way offer-beacon \
  --offer-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --offer-token-name 54657374546f6b656e31 \
  --stdout)

pairBeacon="${beaconPolicyId}.${pairBeaconName}"
offerBeacon="${beaconPolicyId}.${offerBeaconName}"

# Creating the beacon policy redeemer. 
echo "Creating the beacon policy redeemer..."
cardano-swaps beacon-redeemers one-way \
  --burn \
  --out-file $beaconRedeemerFile

# Create the transaction.
echo "Exporting the current protocol parameters..."
cardano-swaps protocol-params \
  --testnet \
  --out-file "${tmpDir}protocol.json"

initial_change=$((7544757570))

echo "Building the initial transaction..."
cardano-cli transaction build-raw \
  --tx-in 520d3be49b112f99bddbcb9a006b05ae23d463837aa4b777c4bec57f088798d5#2 \
  --tx-in 27506ead520a7e7653be5babd9d864c382a5c530b02810029c63366ac042b67e#0 \
  --spending-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(0,0)" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon}" \
  --mint-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#1 \
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
  --tx-in 520d3be49b112f99bddbcb9a006b05ae23d463837aa4b777c4bec57f088798d5#2 \
  --tx-in 27506ead520a7e7653be5babd9d864c382a5c530b02810029c63366ac042b67e#0 \
  --spending-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + ${initial_change} lovelace" \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon}" \
  --mint-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#1 \
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
  --tx-in-count 2 \
  --tx-out-count 2 \
  --witness-count 2 | cut -d' ' -f1)
req_fee=$(($calculated_fee+50000)) # Add 0.05 ADA to be safe since the fee must still be updated.

echo "Rebuilding the transaction with the required fee..."
cardano-cli transaction build-raw \
  --tx-in 520d3be49b112f99bddbcb9a006b05ae23d463837aa4b777c4bec57f088798d5#2 \
  --tx-in 27506ead520a7e7653be5babd9d864c382a5c530b02810029c63366ac042b67e#0 \
  --spending-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-execution-units "(${spend_0_steps},${spend_0_mem})" \
  --spending-reference-tx-in-redeemer-file $swapRedeemerFile \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + 3000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31" \
  --tx-out "$(cat ../../../ignored/wallets/01.addr) + $(($initial_change-$req_fee)) lovelace " \
  --mint "-1 ${pairBeacon} + -1 ${offerBeacon}" \
  --mint-tx-in-reference 64092a335533a4c9b0b85ce6785afe64af2183c7538dfa2c48fe1ebef65b76e3#1 \
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
